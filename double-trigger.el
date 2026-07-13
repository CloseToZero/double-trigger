;;; double-trigger.el --- Trigger a function by pressing two keys quickly -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/double-trigger
;; Version: 0.2.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `double-trigger-mode' runs `double-trigger-fn' when the two
;; characters in `double-trigger-keys' are entered within
;; `double-trigger-delay' seconds.
;;
;; Detection is non-blocking: the first key runs normally.  If the next
;; command is the second key and arrives before the deadline, the first
;; insertion is removed and the second command is replaced by the
;; trigger function.

;;; Code:

(require 'cl-lib)

(defgroup double-trigger nil
  "Trigger a function by pressing two keys quickly."
  :prefix "double-trigger-"
  :group 'keyboard)

(defcustom double-trigger-keys "ii"
  "The two keys to trigger the function `double-trigger-fn'."
  :type 'key-sequence
  :group 'double-trigger)

(defcustom double-trigger-fn nil
  "Function called after pressing `double-trigger-keys' quickly.
The function is called without arguments in place of the second key's
normal command."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'double-trigger)

(defcustom double-trigger-delay 0.2
  "Max time delay between two key presses."
  :type 'number
  :group 'double-trigger)

(defcustom double-trigger-lighter " DT"
  "The lighter for the `double-trigger-mode'."
  :type 'string
  :group 'double-trigger)

(defvar double-trigger-inhibit nil
  "When non-nil double-trigger is inhibited.")

(defvar double-trigger-inhibit-fns nil
  "List of zero argument predicate functions disabling double-trigger.
If any of these functions return non-nil, double-trigger will be
inhibited.")

(defvar double-trigger-insert-fn #'double-trigger-default-insert-fn
  "Compatibility variable retained from double-trigger 0.1.
The non-blocking implementation lets the first key's normal command run,
so this function is no longer called.")

(make-obsolete-variable
 'double-trigger-insert-fn
 "The first key's normal command now performs insertion."
 "0.2.0")

(defvar double-trigger-delete-fn #'double-trigger-default-delete-fn
  "Function used to remove the first trigger character.
By default, `double-trigger-default-delete-fn' deletes the character
before point.  The function is called only after double-trigger has
verified that the first key inserted exactly one character.")

(cl-defstruct (double-trigger--candidate
               (:constructor double-trigger--make-candidate))
  "State captured around a possible first trigger key."
  buffer
  time
  point-before
  size-before
  modified-before
  undo-list-before
  point-after
  tick-after)

(defvar double-trigger--candidate nil
  "Candidate waiting for the second trigger key.")

(defvar double-trigger--pre-command-candidate nil
  "Candidate captured before the current command.")

(defvar double-trigger--trigger-function nil
  "Function to run for the trigger currently being dispatched.")

(defvar double-trigger--skip-post-command nil
  "Non-nil when the current command dispatched a trigger.")

;;;###autoload
(define-minor-mode double-trigger-mode
  "Trigger a function by pressing two keys quickly.
This is a global minor mode."
  :lighter double-trigger-lighter
  :group 'double-trigger
  :global t
  (if double-trigger-mode
      (progn
        (add-hook 'pre-command-hook #'double-trigger--pre-command-hook)
        (add-hook 'post-command-hook #'double-trigger--post-command-hook))
    (remove-hook 'pre-command-hook #'double-trigger--pre-command-hook)
    (remove-hook 'post-command-hook #'double-trigger--post-command-hook)
    (double-trigger--clear-state)))

(defun double-trigger--clear-state ()
  "Discard all pending trigger state."
  (setq double-trigger--candidate nil
        double-trigger--pre-command-candidate nil
        double-trigger--trigger-function nil
        double-trigger--skip-post-command nil))

(defun double-trigger--valid-key-sequence-p ()
  "Return non-nil when `double-trigger-keys' has two characters."
  (and (or (stringp double-trigger-keys)
           (vectorp double-trigger-keys))
       (= (length double-trigger-keys) 2)
       (characterp (elt double-trigger-keys 0))
       (characterp (elt double-trigger-keys 1))))

(defun double-trigger--enabled-p ()
  "Return non-nil when a trigger may be detected now."
  (and (double-trigger--valid-key-sequence-p)
       (functionp double-trigger-fn)
       (numberp double-trigger-delay)
       (>= double-trigger-delay 0)
       (not double-trigger-inhibit)
       (not (cl-some (lambda (fn) (funcall fn))
                     double-trigger-inhibit-fns))))

(defun double-trigger--current-key-p (n)
  "Return non-nil when the current command was trigger key N."
  (let ((keys (this-command-keys-vector)))
    (and (= (length keys) 1)
         (equal (aref keys 0) (elt double-trigger-keys n)))))

(defun double-trigger--now ()
  "Return the current time as a floating-point number."
  (float-time))

(defun double-trigger--capture-candidate (now)
  "Capture buffer state before a possible first key at time NOW."
  (double-trigger--make-candidate
   :buffer (current-buffer)
   :time now
   :point-before (point)
   :size-before (buffer-size)
   :modified-before (buffer-modified-p)
   :undo-list-before buffer-undo-list))

(defun double-trigger--first-key-inserted-p (candidate)
  "Return non-nil when CANDIDATE became one matching insertion."
  (and (eq (current-buffer)
           (double-trigger--candidate-buffer candidate))
       (= (point)
          (1+ (double-trigger--candidate-point-before candidate)))
       (= (buffer-size)
          (1+ (double-trigger--candidate-size-before candidate)))
       (eq (char-before) (elt double-trigger-keys 0))))

(defun double-trigger--candidate-current-p (candidate now)
  "Return non-nil when CANDIDATE is unchanged and valid at NOW."
  (and candidate
       (buffer-live-p (double-trigger--candidate-buffer candidate))
       (eq (current-buffer)
           (double-trigger--candidate-buffer candidate))
       (= (point) (double-trigger--candidate-point-after candidate))
       (= (buffer-chars-modified-tick)
          (double-trigger--candidate-tick-after candidate))
       (let ((elapsed (- now (double-trigger--candidate-time candidate))))
         (and (>= elapsed 0)
              (<= elapsed double-trigger-delay)))))

(defun double-trigger--remove-first-key (candidate)
  "Remove the insertion represented by CANDIDATE.
Return non-nil when the original buffer state was restored."
  (funcall double-trigger-delete-fn)
  (when (and (eq (current-buffer)
                 (double-trigger--candidate-buffer candidate))
             (= (point)
                (double-trigger--candidate-point-before candidate))
             (= (buffer-size)
                (double-trigger--candidate-size-before candidate)))
    (setq buffer-undo-list
          (double-trigger--candidate-undo-list-before candidate))
    (set-buffer-modified-p
     (double-trigger--candidate-modified-before candidate))
    t))

(defun double-trigger--run-trigger ()
  "Run the trigger function selected by the pre-command hook."
  (interactive)
  (let ((fn double-trigger--trigger-function))
    (setq double-trigger--trigger-function nil)
    (when fn
      (funcall fn))))

(defun double-trigger--pre-command-hook ()
  "Detect a second key or capture state before a possible first key."
  (with-demoted-errors "double-trigger: Error %S"
    (setq double-trigger--pre-command-candidate nil
          double-trigger--skip-post-command nil)
    (if (not (double-trigger--enabled-p))
        (setq double-trigger--candidate nil)
      (let* ((first-key-p (double-trigger--current-key-p 0))
             (second-key-p (double-trigger--current-key-p 1))
             (relevant-key-p (or first-key-p second-key-p))
             (now (and relevant-key-p (double-trigger--now)))
             (candidate double-trigger--candidate))
        (setq double-trigger--candidate nil)
        (cond
         ((and second-key-p
               (double-trigger--candidate-current-p candidate now)
               (double-trigger--remove-first-key candidate))
          (setq double-trigger--trigger-function double-trigger-fn
                double-trigger--skip-post-command t
                this-command #'double-trigger--run-trigger
                this-original-command #'double-trigger--run-trigger))
         (first-key-p
          (setq double-trigger--pre-command-candidate
                (double-trigger--capture-candidate now))))))))

(defun double-trigger--post-command-hook ()
  "Arm a candidate after the first key inserted one character."
  (with-demoted-errors "double-trigger: Error %S"
    (let ((candidate double-trigger--pre-command-candidate))
      (setq double-trigger--pre-command-candidate nil)
      (cond
       (double-trigger--skip-post-command
        (setq double-trigger--skip-post-command nil))
       ((and candidate
             (double-trigger--first-key-inserted-p candidate))
        (setf (double-trigger--candidate-point-after candidate) (point)
              (double-trigger--candidate-tick-after candidate)
              (buffer-chars-modified-tick))
        (setq double-trigger--candidate candidate))
       (t
        (setq double-trigger--candidate nil))))))

(defun double-trigger-default-insert-fn ()
  "Insert the current key unless the buffer is read-only.
This function is retained for compatibility with double-trigger 0.1."
  (when (not buffer-read-only) (self-insert-command 1) t))

(defun double-trigger-default-delete-fn ()
  "Delete the character before point unless the buffer is read-only."
  (when (not buffer-read-only) (delete-char -1)))

(provide 'double-trigger)
;;; double-trigger.el ends here
