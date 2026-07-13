;;; double-trigger.el --- Trigger a function by pressing two keys quickly -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/double-trigger
;; Version: 0.4.0
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
;; Detection performs a synchronous lookahead after the first key.  A
;; matching second key replaces the current command with the trigger.
;; A nonmatching event is returned to the normal command loop, after the
;; first key's command has run.  Detection never edits the buffer or
;; rewrites undo history.

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
The function is called without arguments in place of the key pair's
normal commands."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'double-trigger)

(defcustom double-trigger-delay 0.2
  "Maximum time to wait for the second trigger key, in seconds."
  :type 'number
  :group 'double-trigger)

(defcustom double-trigger-lighter " DT"
  "The lighter for `double-trigger-mode'."
  :type 'string
  :group 'double-trigger)

(defvar double-trigger-inhibit nil
  "When non-nil, inhibit Double Trigger detection.")

(defvar double-trigger-inhibit-fns nil
  "List of zero-argument predicate functions disabling Double Trigger.
If any of these functions returns non-nil, detection is inhibited.")

(defvar double-trigger-insert-fn #'double-trigger-default-insert-fn
  "Compatibility variable retained from Double Trigger 0.1.
The lookahead implementation never inserts a candidate key, so this
function is no longer called.")

(make-obsolete-variable
 'double-trigger-insert-fn
 "Candidate keys are handled by their normal commands."
 "0.2.0")

(defvar double-trigger-delete-fn #'double-trigger-default-delete-fn
  "Compatibility variable retained from Double Trigger 0.2.
The lookahead implementation never deletes a candidate key, so this
function is no longer called.")

(make-obsolete-variable
 'double-trigger-delete-fn
 "Candidate keys are handled by their normal commands."
 "0.3.0")

(defvar double-trigger--trigger-function nil
  "Function selected by the pre-command hook for the current trigger.")

;;;###autoload
(define-minor-mode double-trigger-mode
  "Trigger a function by pressing two keys quickly.
This is a global minor mode."
  :lighter double-trigger-lighter
  :group 'double-trigger
  :global t
  (if double-trigger-mode
      (add-hook 'pre-command-hook #'double-trigger--pre-command-hook)
    (remove-hook 'pre-command-hook #'double-trigger--pre-command-hook)
    (double-trigger--clear-state)))

(defun double-trigger--clear-state ()
  "Discard pending trigger state."
  (setq double-trigger--trigger-function nil))

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

(defun double-trigger--self-inserting-command-p ()
  "Return non-nil when the current key would self-insert."
  (or (eq this-command #'self-insert-command)
      (eq this-original-command #'self-insert-command)))

(defun double-trigger--read-event ()
  "Read a possible second trigger event without recording it twice."
  (let ((inhibit--record-char t))
    (read-event nil nil double-trigger-delay)))

(defun double-trigger--restore-event (event)
  "Return nonmatching EVENT to the front of the command input queue."
  (setq unread-command-events (cons event unread-command-events)))

(defun double-trigger--record-trigger-event (event)
  "Record consumed trigger EVENT when defining a keyboard macro."
  (when defining-kbd-macro
    (store-kbd-macro-event event)))

(defun double-trigger--run-trigger ()
  "Run the trigger function selected by the pre-command hook."
  (interactive)
  (let ((fn double-trigger--trigger-function))
    (setq double-trigger--trigger-function nil)
    (when fn
      (funcall fn))))

(defun double-trigger--pre-command-hook ()
  "Detect a configured key pair before running its first command."
  (with-demoted-errors "double-trigger: Error %S"
    (when (and (double-trigger--enabled-p)
               (double-trigger--current-key-p 0)
               (double-trigger--self-inserting-command-p))
      (let ((event (double-trigger--read-event)))
        (cond
         ((and (characterp event)
               (equal event (elt double-trigger-keys 1)))
          (double-trigger--record-trigger-event event)
          (setq double-trigger--trigger-function double-trigger-fn
                this-command #'double-trigger--run-trigger
                this-original-command #'double-trigger--run-trigger))
         (event
          (double-trigger--restore-event event)))))))

(defun double-trigger-default-insert-fn ()
  "Insert the current key unless the buffer is read-only.
This function is retained for compatibility with Double Trigger 0.1."
  (when (not buffer-read-only)
    (self-insert-command 1)
    t))

(defun double-trigger-default-delete-fn ()
  "Delete the character before point unless the buffer is read-only.
This function is retained for compatibility with Double Trigger 0.2."
  (when (not buffer-read-only)
    (delete-char -1)))

(provide 'double-trigger)
;;; double-trigger.el ends here
