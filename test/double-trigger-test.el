;;; double-trigger-test.el --- Tests for double-trigger -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Zhexuan Chen <2915234902@qq.com>

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'double-trigger)

(defmacro double-trigger-test--with-buffer (&rest body)
  "Run BODY in an isolated buffer with double-trigger enabled."
  (declare (indent 0) (debug t))
  `(let ((double-trigger-keys "ii")
         (double-trigger-fn nil)
         (double-trigger-delay 0.2)
         (double-trigger-inhibit nil)
         (double-trigger-inhibit-fns nil)
         (double-trigger-delete-fn #'double-trigger-default-delete-fn)
         (unread-command-events nil)
         (unread-post-input-method-events nil)
         (unread-input-method-events nil))
     (unwind-protect
         (with-temp-buffer
           (let ((test-buffer (current-buffer)))
             (save-window-excursion
               (switch-to-buffer test-buffer)
               (setq buffer-undo-list nil)
               (double-trigger-mode 1)
               ,@body)))
       (double-trigger--clear-state)
       (setq unread-command-events nil
             unread-post-input-method-events nil
             unread-input-method-events nil)
       (double-trigger-mode -1))))

(defmacro double-trigger-test--with-times (times &rest body)
  "Return successive TIMES from the double-trigger clock while running BODY."
  (declare (indent 1) (debug t))
  `(let ((remaining-times ,times))
     (cl-letf (((symbol-function 'double-trigger--now)
                (lambda ()
                  (if remaining-times
                      (pop remaining-times)
                    (ert-fail "double-trigger read the clock too often")))))
       ,@body
       (should-not remaining-times))))

(defun double-trigger-test--expire-candidate ()
  "Expire and replay the current candidate through the command loop."
  (when double-trigger--candidate
    (double-trigger--timeout double-trigger--candidate)
    (execute-kbd-macro [])))

(ert-deftest double-trigger-fast-sequence-is-nonblocking ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1)
        (cl-letf (((symbol-function 'read-event)
                   (lambda (&rest _)
                     (ert-fail "double-trigger called read-event")))
                  ((symbol-function 'store-kbd-macro-event)
                   (lambda (&rest _)
                     (ert-fail "double-trigger recorded an event manually"))))
          (execute-kbd-macro (vector ?i ?i))))
      (should (= trigger-count 1))
      (should (equal (buffer-string) ""))
      (should-not (buffer-modified-p))
      (should-not buffer-undo-list)
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-slow-sequence-inserts-both-keys ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.3 0.3)
        (execute-kbd-macro (vector ?i ?i)))
      (double-trigger-test--expire-candidate)
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ii"))
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-intervening-command-cancels-candidate ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1)
        (execute-kbd-macro (vector ?i ?a ?i)))
      (double-trigger-test--expire-candidate)
      (should (= trigger-count 0))
      (should (equal (buffer-string) "iai")))))

(ert-deftest double-trigger-inhibit-function-prevents-detection ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count)))
            double-trigger-inhibit-fns (list (lambda () t)))
      (execute-kbd-macro (vector ?i ?i))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ii")))))

(ert-deftest double-trigger-intervening-editing-command-preserves-order ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1)
        (execute-kbd-macro (vector ?i ?x ?i)))
      (double-trigger-test--expire-candidate)
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ixi")))))

(ert-deftest double-trigger-non-inserting-command-does-not-arm ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (local-set-key "i" #'ignore)
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro (vector ?i ?i))
      (should (= trigger-count 0))
      (should (equal (buffer-string) ""))
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-timeout-replays-first-key ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0)
        (execute-kbd-macro (vector ?i)))
      (should (equal (buffer-string) ""))
      (double-trigger-test--expire-candidate)
      (should (= trigger-count 0))
      (should (equal (buffer-string) "i"))
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-real-timer-replays-first-key ()
  (double-trigger-test--with-buffer
    (let ((double-trigger-delay 0.01)
          (trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro (vector ?i))
      (should (equal (buffer-string) ""))
      (let ((deadline (+ (float-time) 0.5)))
        (while (and double-trigger--candidate
                    (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (execute-kbd-macro [])
      (should (= trigger-count 0))
      (should (equal (buffer-string) "i"))
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-pending-fast-pair-survives-command-lag ()
  (double-trigger-test--with-buffer
    (let ((double-trigger-delay 0.01)
          pending-before-stall
          stalled
          (trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (local-set-key [f13] #'exit-recursive-edit)
      (add-hook 'post-command-hook
                (lambda ()
                  (when (and double-trigger--candidate
                             (not stalled))
                    (setq stalled t
                          pending-before-stall (input-pending-p))
                    (sleep-for 0.02)))
                nil t)
      (setq unread-command-events (list ?i ?i 'f13))
      (recursive-edit)
      (should stalled)
      (should pending-before-stall)
      (should (= trigger-count 1))
      (should (equal (buffer-string) ""))
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-pending-nonmatch-preserves-event-order ()
  (double-trigger-test--with-buffer
    (let ((double-trigger-delay 0.01)
          pending-before-stall
          stalled
          (trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (local-set-key [f13] #'exit-recursive-edit)
      (add-hook 'post-command-hook
                (lambda ()
                  (when (and double-trigger--candidate
                             (not stalled))
                    (setq stalled t
                          pending-before-stall (input-pending-p))
                    (sleep-for 0.02)))
                nil t)
      (setq unread-command-events (list ?i ?x 'f13))
      (recursive-edit)
      (should stalled)
      (should pending-before-stall)
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ix"))
      (should-not double-trigger--candidate))))

(ert-deftest double-trigger-replay-bypasses-input-method ()
  (double-trigger-test--with-buffer
    (let ((input-method-calls 0)
          (trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (setq-local input-method-function
                  (lambda (event)
                    (setq input-method-calls (1+ input-method-calls))
                    (list event)))
      (setq unread-input-method-events (list ?i))
      (execute-kbd-macro [])
      (should (= input-method-calls 1))
      (double-trigger-test--expire-candidate)
      (should (= input-method-calls 1))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "i")))))

(ert-deftest double-trigger-existing-undo-history-remains-valid ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro "abc")
      (double-trigger-test--with-times '(0.0 0.1)
        (execute-kbd-macro (vector ?i ?i)))
      (should (= trigger-count 1))
      (should (equal (buffer-string) "abc"))
      (undo-boundary)
      (let ((inhibit-message t))
        (undo-only 1))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-replay-preserves-self-insert-undo-group ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0)
        (execute-kbd-macro "abcix"))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "abcix"))
      (undo-boundary)
      (let ((inhibit-message t))
        (undo-only 1))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-overlapping-third-key-is-replayed ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1 0.2)
        (execute-kbd-macro (vector ?i ?i ?i)))
      (double-trigger-test--expire-candidate)
      (should (= trigger-count 1))
      (should (equal (buffer-string) "i")))))

(ert-deftest double-trigger-vundo-can-traverse-existing-branches ()
  (skip-unless (require 'vundo nil t))
  (double-trigger-test--with-buffer
    (let ((trigger-count 0)
          (original-buffer (current-buffer))
          vundo-buffer)
      (unwind-protect
          (progn
            (setq double-trigger-fn
                  (lambda () (setq trigger-count (1+ trigger-count))))
            (execute-kbd-macro "abc")
            (undo-boundary)
            (let ((inhibit-message t))
              (undo-only 1))
            (undo-boundary)
            (execute-kbd-macro "x")
            (double-trigger-test--with-times '(0.0 0.1)
              (execute-kbd-macro (vector ?i ?i)))
            (should (= trigger-count 1))
            (should (equal (buffer-string) "x"))
            (setq vundo-buffer (vundo-1 original-buffer))
            (with-current-buffer vundo-buffer
              (let* ((mod-list vundo--prev-mod-list)
                     (root (aref mod-list 0))
                     (current (vundo--current-node mod-list))
                     (branches (vundo-m-children root))
                     (other (car (delq current (copy-sequence branches)))))
                (should (= (length branches) 2))
                (should other)
                (vundo--move-to-node current other original-buffer mod-list)))
            (with-current-buffer original-buffer
              (should (equal (buffer-string) "abc"))))
        (when (buffer-live-p vundo-buffer)
          (kill-buffer vundo-buffer))))))

(ert-deftest double-trigger-keyboard-macro-replays-once-per-pair ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0)
          (macro (vector ?i ?i)))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1 1.0 1.1)
        (execute-kbd-macro macro)
        (execute-kbd-macro macro))
      (should (= trigger-count 2))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-replay-does-not-duplicate-recorded-macro-events ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (local-set-key [f13] #'exit-recursive-edit)
      (local-set-key [f14] #'end-kbd-macro)
      (start-kbd-macro nil)
      (unwind-protect
          (double-trigger-test--with-times '(0.0)
            (setq unread-input-method-events (list ?i ?x 'f14 'f13))
            (recursive-edit))
        (when defining-kbd-macro
          (end-kbd-macro)))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ix"))
      (should (equal last-kbd-macro (vector ?i ?x))))))

(ert-deftest double-trigger-pair-is-recorded-once-in-keyboard-macro ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (local-set-key [f13] #'exit-recursive-edit)
      (local-set-key [f14] #'end-kbd-macro)
      (start-kbd-macro nil)
      (unwind-protect
          (double-trigger-test--with-times '(0.0 0.1)
            (setq unread-input-method-events (list ?i ?i 'f14 'f13))
            (recursive-edit))
        (when defining-kbd-macro
          (end-kbd-macro)))
      (should (= trigger-count 1))
      (should (equal (buffer-string) ""))
      (should (equal last-kbd-macro (vector ?i ?i))))))

(provide 'double-trigger-test)
;;; double-trigger-test.el ends here
