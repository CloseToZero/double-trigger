;;; double-trigger-test.el --- Tests for double-trigger -*- lexical-binding: t; -*-

;; Copyright (c) 2023-2026 Zhexuan Chen <2915234902@qq.com>

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'double-trigger)

(defmacro double-trigger-test--with-buffer (&rest body)
  "Run BODY in an isolated buffer with Double Trigger enabled."
  (declare (indent 0) (debug t))
  `(let ((double-trigger-keys "ii")
         (double-trigger-fn nil)
         (double-trigger-delay 0.2)
         (double-trigger-inhibit nil)
         (double-trigger-inhibit-fns nil)
         (unread-command-events nil)
         (unread-post-input-method-events nil)
         (unread-input-method-events nil))
     (unwind-protect
         (with-temp-buffer
           (let ((test-buffer (current-buffer)))
             (save-window-excursion
               (switch-to-buffer test-buffer)
               (when (bound-and-true-p evil-local-mode)
                 (evil-local-mode -1))
               (setq buffer-undo-list nil)
               (double-trigger-mode 1)
               ,@body)))
       (double-trigger--clear-state)
       (setq unread-command-events nil
             unread-post-input-method-events nil
             unread-input-method-events nil)
       (double-trigger-mode -1))))

(ert-deftest double-trigger-fast-sequence-runs-trigger ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro (vector ?i ?i))
      (should (= trigger-count 1))
      (should (equal (buffer-string) ""))
      (should-not (buffer-modified-p))
      (should-not buffer-undo-list))))

(ert-deftest double-trigger-delay-is-configurable ()
  (double-trigger-test--with-buffer
    (let ((double-trigger-delay 0.137)
          observed-delay)
      (setq double-trigger-fn #'ignore)
      (cl-letf (((symbol-function 'read-event)
                 (lambda (_prompt _inherit delay)
                   (setq observed-delay delay)
                   nil)))
        (execute-kbd-macro "i"))
      (should (= observed-delay 0.137))
      (should (equal (buffer-string) "i")))))

(ert-deftest double-trigger-slow-sequence-inserts-both-keys ()
  (double-trigger-test--with-buffer
    (let ((events '(nil nil))
          (trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (cl-letf (((symbol-function 'double-trigger--read-event)
                 (lambda () (pop events))))
        (execute-kbd-macro (vector ?i ?i)))
      (should-not events)
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ii")))))

(ert-deftest double-trigger-lookahead-does-not-edit-buffer ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (cl-letf (((symbol-function 'double-trigger--read-event)
                 (lambda ()
                   (should (equal (buffer-string) ""))
                   (should-not (buffer-modified-p))
                   (should-not buffer-undo-list)
                   ?i)))
        (execute-kbd-macro "i"))
      (should (= trigger-count 1))
      (should (equal (buffer-string) ""))
      (should-not buffer-undo-list))))

(ert-deftest double-trigger-nonmatch-preserves-event-order ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro (vector ?i ?x ?y))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ixy")))))

(ert-deftest double-trigger-inhibit-function-prevents-detection ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count)))
            double-trigger-inhibit-fns (list (lambda () t)))
      (cl-letf (((symbol-function 'read-event)
                 (lambda (&rest _)
                   (ert-fail "Double Trigger read an inhibited event"))))
        (execute-kbd-macro (vector ?i ?i)))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ii")))))

(ert-deftest double-trigger-non-inserting-command-does-not-detect ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (local-set-key "i" #'ignore)
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (cl-letf (((symbol-function 'read-event)
                 (lambda (&rest _)
                   (ert-fail "Double Trigger read after a command"))))
        (execute-kbd-macro (vector ?i ?i)))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-detects-after-state-transition ()
  (double-trigger-test--with-buffer
    (let ((insert-state nil)
          (trigger-count 0))
      (local-set-key [f12]
                     (lambda ()
                       (interactive)
                       (setq insert-state t)))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count)))
            double-trigger-inhibit-fns
            (list (lambda () (not insert-state))))
      (execute-kbd-macro (vector 'f12 ?i ?i))
      (should insert-state)
      (should (= trigger-count 1))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-completion-sees-first-nonmatching-key ()
  (double-trigger-test--with-buffer
    (let (content-seen-by-completion)
      (setq double-trigger-fn #'ignore)
      (local-set-key [tab]
                     (lambda ()
                       (interactive)
                       (setq content-seen-by-completion (buffer-string))))
      (execute-kbd-macro (vector ?i 'tab))
      (should (equal content-seen-by-completion "i"))
      (should (equal (buffer-string) "i")))))

(ert-deftest double-trigger-existing-undo-history-remains-valid ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro "abc")
      (execute-kbd-macro (vector ?i ?i))
      (should (= trigger-count 1))
      (should (equal (buffer-string) "abc"))
      (undo-boundary)
      (let ((inhibit-message t))
        (undo-only 1))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-nonmatch-preserves-self-insert-undo-group ()
  (double-trigger-test--with-buffer
    (setq double-trigger-fn #'ignore)
    (execute-kbd-macro "abcix")
    (should (equal (buffer-string) "abcix"))
    (undo-boundary)
    (let ((inhibit-message t))
      (undo-only 1))
    (should (equal (buffer-string) ""))))

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
            (execute-kbd-macro (vector ?i ?i))
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

(ert-deftest double-trigger-keyboard-macro-runs-once-per-pair ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0)
          (macro (vector ?i ?i)))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (execute-kbd-macro macro)
      (execute-kbd-macro macro)
      (should (= trigger-count 2))
      (should (equal (buffer-string) "")))))

(ert-deftest double-trigger-nonmatch-is-recorded-once-in-keyboard-macro ()
  (double-trigger-test--with-buffer
    (setq double-trigger-fn #'ignore)
    (local-set-key [f13] #'exit-recursive-edit)
    (local-set-key [f14] #'end-kbd-macro)
    (start-kbd-macro nil)
    (unwind-protect
        (progn
          (setq unread-input-method-events (list ?i ?x 'f14 'f13))
          (recursive-edit))
      (when defining-kbd-macro
        (end-kbd-macro)))
    (should (equal (buffer-string) "ix"))
    (should (equal last-kbd-macro (vector ?i ?x)))))

(ert-deftest double-trigger-pair-is-recorded-once-in-keyboard-macro ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (local-set-key [f13] #'exit-recursive-edit)
      (local-set-key [f14] #'end-kbd-macro)
      (start-kbd-macro nil)
      (unwind-protect
          (progn
            (setq unread-input-method-events (list ?i ?i 'f14 'f13))
            (recursive-edit))
        (when defining-kbd-macro
          (end-kbd-macro)))
      (should (= trigger-count 1))
      (should (equal (buffer-string) ""))
      (should (equal last-kbd-macro (vector ?i ?i))))))

(provide 'double-trigger-test)
;;; double-trigger-test.el ends here
