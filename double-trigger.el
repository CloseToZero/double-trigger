;;; double-trigger.el --- Trigger a function by pressing two keys quickly -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2024 Zhexuan Chen <2915234902@qq.com>

;; Author: Zhexuan Chen <2915234902@qq.com>
;; URL: https://github.com/CloseToZero/double-trigger
;; Version: 0.1.0
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

;; TODO

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
  "The zero argument function trigger by pressing \
`double-trigger-keys' quickly."
  :type 'sexp
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
  "The zero argument function used to insert the character
corresponding to the current key.

The function should return non-nil if it have inserted a character.

By default, the insert function is `double-trigger-default-insert-fn'
which check if the buffer is readonly and then use
\(self-insert-command 1)' to insert the character.  You can set a
custom function using different insertion methods for different
situations (e.g. use `term-send-raw' in `term-mode').")

(defvar double-trigger-delete-fn #'double-trigger-default-delete-fn
  "The zero argument function used to delete the character inserted by
`double-trigger-insert-fn'.

By default, the delete function is `double-trigger-default-delete-fn'
which check if the buffer is readonly and then use
\(delete-char -1)' to delete the character.  You can set a
custom function using different deleteion methods for different
situations (e.g. use `term-send-backspace' in `term-mode').")

;;;###autoload
(define-minor-mode double-trigger-mode
  "Buffer-local minor mode to trigger a function by pressing two keys quickly."
  :lighter double-trigger-lighter
  :group 'double-trigger
  :global t
  (if double-trigger-mode
      (add-hook 'pre-command-hook #'double--trigger-pre-command-hook)
    (remove-hook 'pre-command-hook #'double--trigger-pre-command-hook)))

(defun double--trigger-pre-command-hook ()
  (with-demoted-errors "double-trigger: Error %S"
    (when (double-trigger--trigger?)
      (let* ((old-modified (buffer-modified-p))
             (inserted (funcall double-trigger-insert-fn))
             (event (let ((inhibit--record-char t))
                      (read-event nil nil double-trigger-delay))))
        (when inserted (funcall double-trigger-delete-fn))
        (set-buffer-modified-p old-modified)
        (cond ((and (characterp event)
                    (equal (this-command-keys)
                           (double-trigger--nth-key-str 0))
                    (equal event (elt double-trigger-keys 1)))
               (store-kbd-macro-event event)
               (funcall double-trigger-fn))
              ((null event) nil)
              (t (setq unread-command-events
                       (append unread-command-events (list event)))))))))

(defun double-trigger--trigger? ()
  (and double-trigger-keys
       double-trigger-fn
       (not double-trigger-inhibit)
       (equal (this-command-keys) (double-trigger--nth-key-str 0))
       (not (cl-some (lambda (fn) (funcall fn))
                     double-trigger-inhibit-fns))))

(defun double-trigger--nth-key-str (n)
  (char-to-string (elt double-trigger-keys n)))

(defun double-trigger-default-insert-fn ()
  (when (not buffer-read-only) (self-insert-command 1) t))

(defun double-trigger-default-delete-fn ()
  (when (not buffer-read-only) (delete-char -1)))

(provide 'double-trigger)
