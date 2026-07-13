;;; double-trigger.el --- Trigger a function by pressing two keys quickly -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/double-trigger
;; Version: 0.3.0
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
;; Detection is non-blocking and does not edit undo history.  The first
;; key is deferred.  If the next command is the second key and arrives
;; before the deadline, the pair is replaced by the trigger function.
;; Otherwise the first key is replayed through the normal command loop.

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
The event-replay implementation lets the first key's normal command run,
so this function is no longer called.")

(make-obsolete-variable
 'double-trigger-insert-fn
 "The first key's normal command now performs insertion when replayed."
 "0.2.0")

(defvar double-trigger-delete-fn #'double-trigger-default-delete-fn
  "Compatibility variable retained from double-trigger 0.2.
The event-replay implementation never inserts and then removes a
candidate key, so this function is no longer called.")

(make-obsolete-variable
 'double-trigger-delete-fn
 "Candidate keys are deferred instead of being deleted."
 "0.3.0")

(cl-defstruct (double-trigger--candidate
               (:constructor double-trigger--make-candidate))
  "State for a deferred first trigger key."
  buffer
  window
  time
  delay
  point
  tick
  event
  second-event
  grace
  timer)

(defvar double-trigger--candidate nil
  "Candidate waiting for the second trigger key.")

(defvar double-trigger--trigger-function nil
  "Function to run for the trigger currently being dispatched.")

(defvar double-trigger--replay-keys nil
  "Key vectors queued for replay without trigger detection.")

(defvar double-trigger--restore-command nil
  "Command identity restored after an internal no-op command.")

(defvar double-trigger--restore-original-command nil
  "Original command identity restored after an internal no-op command.")

(defconst double-trigger--pending-input-poll-delay 0.01
  "Seconds between checks while command input is already pending.")

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
        (add-hook 'post-command-hook #'double-trigger--post-command-hook t))
    (remove-hook 'pre-command-hook #'double-trigger--pre-command-hook)
    (remove-hook 'post-command-hook #'double-trigger--post-command-hook)
    (double-trigger--clear-state)))

(defun double-trigger--clear-state ()
  "Discard all pending trigger state."
  (let ((candidate double-trigger--candidate))
    (when candidate
      (double-trigger--cancel-timer candidate)))
  (setq double-trigger--candidate nil
        double-trigger--trigger-function nil
        double-trigger--replay-keys nil
        double-trigger--restore-command nil
        double-trigger--restore-original-command nil))

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
  (double-trigger--current-event-p (elt double-trigger-keys n)))

(defun double-trigger--current-event-p (event)
  "Return non-nil when the current command consists of EVENT."
  (let ((keys (this-command-keys-vector)))
    (and (= (length keys) 1)
         (equal (aref keys 0) event))))

(defun double-trigger--now ()
  "Return the current time as a floating-point number."
  (float-time))

(defun double-trigger--self-inserting-command-p ()
  "Return non-nil when the current key would self-insert."
  (or (eq this-command #'self-insert-command)
      (eq this-original-command #'self-insert-command)))

(defun double-trigger--candidate-current-p (candidate now)
  "Return non-nil when deferred CANDIDATE is valid at NOW."
  (and candidate
       (buffer-live-p (double-trigger--candidate-buffer candidate))
       (eq (current-buffer)
           (double-trigger--candidate-buffer candidate))
       (eq (selected-window)
           (double-trigger--candidate-window candidate))
       (= (point) (double-trigger--candidate-point candidate))
       (= (buffer-chars-modified-tick)
          (double-trigger--candidate-tick candidate))
       (or (double-trigger--candidate-grace candidate)
           (let ((elapsed (- now
                             (double-trigger--candidate-time candidate))))
             (and (>= elapsed 0)
                  (<= elapsed
                      (double-trigger--candidate-delay candidate)))))))

(defun double-trigger--cancel-timer (candidate)
  "Cancel CANDIDATE's timeout timer."
  (let ((timer (double-trigger--candidate-timer candidate)))
    (when timer
      (cancel-timer timer)
      (setf (double-trigger--candidate-timer candidate) nil))))

(defun double-trigger--queue-events (events replay-keys)
  "Queue EVENTS and bypass detection for REPLAY-KEYS.
EVENTS is a list of input events.  REPLAY-KEYS is a list of key
vectors corresponding to commands which must run normally.  Queue the
events after input-method processing so they are not translated twice."
  (setq double-trigger--replay-keys
        (append replay-keys double-trigger--replay-keys)
        unread-post-input-method-events
        (append events
                (append unread-post-input-method-events nil))))

(defun double-trigger--replay-command-p ()
  "Consume and return non-nil for the next queued replay command."
  (when double-trigger--replay-keys
    (let ((keys (this-command-keys-vector)))
      (if (equal keys (car double-trigger--replay-keys))
          (progn
            (setq double-trigger--replay-keys
                  (cdr double-trigger--replay-keys))
            t)
        (setq double-trigger--replay-keys nil)
        nil))))

(defun double-trigger--release-candidate (candidate &optional keys)
  "Replay CANDIDATE before optional current command KEYS."
  (double-trigger--cancel-timer candidate)
  (setq double-trigger--candidate nil)
  (let ((event (double-trigger--candidate-event candidate)))
    (double-trigger--queue-events
     (cons event (append keys nil))
     (list (vector event)))))

(defun double-trigger--timeout (candidate)
  "Replay CANDIDATE after its detection window expires."
  (when (eq candidate double-trigger--candidate)
    (setf (double-trigger--candidate-timer candidate) nil)
    (if (input-pending-p)
        (setf (double-trigger--candidate-grace candidate) t
              (double-trigger--candidate-timer candidate)
              (run-at-time double-trigger--pending-input-poll-delay nil
                           #'double-trigger--timeout candidate))
      (double-trigger--release-candidate candidate))))

(defun double-trigger--arm-candidate (now)
  "Defer the current first trigger key at time NOW."
  (let* ((candidate
          (double-trigger--make-candidate
           :buffer (current-buffer)
           :window (selected-window)
           :time now
           :delay double-trigger-delay
           :point (point)
           :tick (buffer-chars-modified-tick)
           :event (elt double-trigger-keys 0)
           :second-event (elt double-trigger-keys 1)))
         (timer (run-at-time (double-trigger--candidate-delay candidate) nil
                             #'double-trigger--timeout candidate)))
    (setf (double-trigger--candidate-timer candidate) timer)
    (setq double-trigger--candidate candidate
          double-trigger--restore-command this-command
          double-trigger--restore-original-command this-original-command
          this-command #'double-trigger--ignore-command
          this-original-command #'double-trigger--ignore-command)))

(defun double-trigger--ignore-command ()
  "Do nothing while deferred input is queued for replay."
  (interactive))

(declare-function evil-set-command-property "evil-common"
                  (command property value))

(with-eval-after-load 'evil
  ;; Evil records unannotated commands as repeatable keystrokes.  This
  ;; internal command consumes an event only so it can be replayed later.
  (when (fboundp 'evil-set-command-property)
    (evil-set-command-property
     #'double-trigger--ignore-command :repeat 'ignore)))

(defun double-trigger--run-trigger ()
  "Run the trigger function selected by the pre-command hook."
  (interactive)
  (let ((fn double-trigger--trigger-function))
    (setq double-trigger--trigger-function nil)
    (when fn
      (funcall fn))))

(defun double-trigger--pre-command-hook ()
  "Detect a pair or defer and replay a possible first key."
  (with-demoted-errors "double-trigger: Error %S"
    (cond
     ((double-trigger--replay-command-p))
     (double-trigger--candidate
      (let* ((candidate double-trigger--candidate)
             (keys (this-command-keys-vector))
             (second-key-p
              (double-trigger--current-event-p
               (double-trigger--candidate-second-event candidate)))
             (enabled-p (double-trigger--enabled-p))
             (now (and second-key-p enabled-p (double-trigger--now))))
        (if (and now
                 (double-trigger--candidate-current-p candidate now))
            (progn
              (double-trigger--cancel-timer candidate)
              (setq double-trigger--candidate nil
                    double-trigger--trigger-function double-trigger-fn
                    this-command #'double-trigger--run-trigger
                    this-original-command #'double-trigger--run-trigger))
          (double-trigger--release-candidate candidate keys)
          (setq double-trigger--restore-command last-command
                double-trigger--restore-original-command last-command
                this-command #'double-trigger--ignore-command
                this-original-command #'double-trigger--ignore-command))))
     ((and (double-trigger--enabled-p)
           (double-trigger--current-key-p 0)
           (double-trigger--self-inserting-command-p))
      (double-trigger--arm-candidate (double-trigger--now))))))

(defun double-trigger--post-command-hook ()
  "Restore command identity after an internal no-op command."
  (with-demoted-errors "double-trigger: Error %S"
    (when double-trigger--restore-command
      (when (and double-trigger--candidate
                 (not executing-kbd-macro)
                 (input-pending-p))
        ;; The next physical event arrived before this command finished.
        ;; Do not let command-loop work make that pair look slow.
        (setf (double-trigger--candidate-grace double-trigger--candidate) t))
      (setq this-command double-trigger--restore-command
            this-original-command double-trigger--restore-original-command
            double-trigger--restore-command nil
            double-trigger--restore-original-command nil)
      (when (and (eq this-command #'self-insert-command)
                 (fboundp 'undo-auto-amalgamate))
        (undo-auto-amalgamate)))))

(defun double-trigger-default-insert-fn ()
  "Insert the current key unless the buffer is read-only.
This function is retained for compatibility with double-trigger 0.1."
  (when (not buffer-read-only) (self-insert-command 1) t))

(defun double-trigger-default-delete-fn ()
  "Delete the character before point unless the buffer is read-only."
  (when (not buffer-read-only) (delete-char -1)))

(provide 'double-trigger)
;;; double-trigger.el ends here
