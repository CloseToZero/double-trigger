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
         (double-trigger-delete-fn #'double-trigger-default-delete-fn))
     (unwind-protect
         (with-temp-buffer
           (let ((test-buffer (current-buffer)))
             (save-window-excursion
               (switch-to-buffer test-buffer)
               (setq buffer-undo-list nil)
               (double-trigger-mode 1)
               ,@body)))
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
      (double-trigger-test--with-times '(0.0 0.3)
        (execute-kbd-macro (vector ?i ?i)))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ii"))
      (should double-trigger--candidate))))

(ert-deftest double-trigger-intervening-command-cancels-candidate ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1)
        (execute-kbd-macro (vector ?i ?a ?i)))
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

(ert-deftest double-trigger-buffer-change-cancels-candidate ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1)
        (execute-kbd-macro (vector ?i))
        (insert "x")
        (execute-kbd-macro (vector ?i)))
      (should (= trigger-count 0))
      (should (equal (buffer-string) "ixi")))))

(ert-deftest double-trigger-non-inserting-command-does-not-arm ()
  (double-trigger-test--with-buffer
    (let ((trigger-count 0))
      (local-set-key "i" #'ignore)
      (setq double-trigger-fn
            (lambda () (setq trigger-count (1+ trigger-count))))
      (double-trigger-test--with-times '(0.0 0.1)
        (execute-kbd-macro (vector ?i ?i)))
      (should (= trigger-count 0))
      (should (equal (buffer-string) ""))
      (should-not double-trigger--candidate))))

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

(provide 'double-trigger-test)
;;; double-trigger-test.el ends here
