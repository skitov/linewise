;;; linewise-test.el --- Tests for linewise -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run with: emacs -Q --batch -l test/linewise-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the source explicitly so a stale local .elc file cannot affect tests.
(load-file (expand-file-name "../linewise.el"
                              (file-name-directory (or load-file-name buffer-file-name))))

(defmacro linewise-test-with-buffer (contents &rest body)
  "Evaluate BODY in a temporary buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((transient-mark-mode t))
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

(defun linewise-test-select (point-position mark-position)
  "Select the region from MARK-POSITION to POINT-POSITION."
  (goto-char point-position)
  (set-mark mark-position)
  (setq mark-active t))

(ert-deftest linewise-count-lines-current-line-without-region ()
  (linewise-test-with-buffer "one\ntwo\nthree\n"
    (goto-char 6)
    (should (= 1 (linewise-count-lines-region-with-empty-last)))
    (should (equal "two\n" (linewise-affected-lines-content)))))

(ert-deftest linewise-count-lines-includes-empty-final-region-line ()
  (linewise-test-with-buffer "one\ntwo\nthree\n"
    (linewise-test-select 9 5)
    (should (= 2 (linewise-count-lines-region-with-empty-last)))
    (should (equal "two\nthree\n" (linewise-affected-lines-content)))))

(ert-deftest linewise-affected-lines-is-independent-of-region-direction ()
  (dolist (selection '((6 11) (11 6)))
    (linewise-test-with-buffer "one\ntwo\nthree\nfour\n"
      (apply #'linewise-test-select selection)
      (should (equal "two\nthree\n" (linewise-affected-lines-content))))))

(ert-deftest linewise-affected-lines-keeps-final-line-without-newline ()
  (linewise-test-with-buffer "one\ntwo"
    (goto-char (point-max))
    (should (equal "two" (linewise-affected-lines-content)))))

(ert-deftest linewise-delete-removes-current-line-only ()
  (linewise-test-with-buffer "one\ntwo\nthree\n"
    (goto-char 6)
    (linewise-delete)
    (should (equal "one\nthree\n" (buffer-string)))))

(ert-deftest linewise-delete-removes-all-selected-lines ()
  (linewise-test-with-buffer "one\ntwo\nthree\nfour\n"
    (linewise-test-select 11 6)
    (linewise-delete)
    (should (equal "one\nfour\n" (buffer-string)))))

(ert-deftest linewise-copy-saves-selected-lines-to-kill-ring ()
  (let ((kill-ring nil))
    (linewise-test-with-buffer "one\ntwo\nthree\n"
      (linewise-test-select 11 6)
      (linewise-copy)
      (should (equal "two\nthree\n" (current-kill 0)))
      (should (equal "one\ntwo\nthree\n" (buffer-string))))))

(ert-deftest linewise-kill-removes-and-saves-current-line ()
  (let ((kill-ring nil))
    (linewise-test-with-buffer "one\ntwo\nthree\n"
      (goto-char 6)
      (linewise-kill)
      (should (equal "two\n" (current-kill 0)))
      (should (equal "one\nthree\n" (buffer-string))))))

(ert-deftest linewise-yank-inserts-before-current-line ()
  (let ((kill-ring '("inserted\n")))
    (linewise-test-with-buffer "one\ntwo\n"
      (goto-char 6)
      (linewise-yank)
      (should (equal "one\ninserted\ntwo\n" (buffer-string))))))

(ert-deftest linewise-repeat-duplicates-current-line ()
  (linewise-test-with-buffer "one\ntwo\n"
    (goto-char 2)
    (linewise-repeat 2)
    (should (equal "one\none\none\ntwo\n" (buffer-string)))))

(ert-deftest linewise-repeat-duplicates-selected-lines ()
  (linewise-test-with-buffer "one\ntwo\nthree\nfour\n"
    (linewise-test-select 11 6)
    (linewise-repeat 1)
    (should (equal "one\ntwo\nthree\ntwo\nthree\nfour\n" (buffer-string)))))

(ert-deftest linewise-move-up-or-down-moves-current-line-in-both-directions ()
  (linewise-test-with-buffer "one\ntwo\nthree\n"
    (goto-char 6)
    (linewise-move-up-or-down 1)
    (should (equal "one\nthree\ntwo\n" (buffer-string)))
    (linewise-move-up-or-down 0)
    (should (equal "one\ntwo\nthree\n" (buffer-string)))))

(ert-deftest linewise-move-up-or-down-keeps-multiple-lines-together ()
  (linewise-test-with-buffer "one\ntwo\nthree\nfour\nfive\n"
    (linewise-test-select 11 6)
    (linewise-move-up-or-down 2)
    (should (equal "one\nfour\nfive\ntwo\nthree\n" (buffer-string)))))

(ert-deftest linewise-move-up-or-down-does-not-cross-buffer-boundaries ()
  (linewise-test-with-buffer "one\ntwo\n"
    (goto-char 2)
    (linewise-move-up-or-down -1)
    (should (equal "one\ntwo\n" (buffer-string)))
    (goto-char (point-max))
    (linewise-move-up-or-down 1)
    (should (equal "one\ntwo\n" (buffer-string)))))

(ert-deftest linewise-narrow-limits-buffer-to-selected-lines ()
  (linewise-test-with-buffer "one\ntwo\nthree\nfour\n"
    (linewise-test-select 11 6)
    (linewise-narrow)
    (should (buffer-narrowed-p))
    (should (equal "two\nthree\n" (buffer-string)))))

(ert-deftest linewise-insert-select-selects-inserted-multiple-lines ()
  (linewise-test-with-buffer "before\n"
    (goto-char (point-max))
    (linewise-insert-select "one\ntwo\n")
    (should (equal "before\none\ntwo\n" (buffer-string)))
    (should mark-active)
    (should (equal "one\ntwo\n" (linewise-affected-lines-content)))))

(ert-deftest linewise-set-keybindings-registers-all-commands ()
  (let (calls)
    (cl-letf (((symbol-function 'global-set-key)
               (lambda (key command) (push (cons key command) calls))))
      (linewise-set-keybindings "C-c l"))
    (dolist (binding '(("M-N" . linewise-move-up-or-down)
                       ("M-P" . linewise-move-up)
                       ("C-c l C-n" . linewise-move-down-fast)
                       ("C-c l C-p" . linewise-move-up-fast)
                       ("C-c l k" . linewise-kill)
                       ("C-c l d" . linewise-delete)
                       ("C-c l c" . linewise-copy)
                       ("C-c l y" . linewise-yank)
                       ("C-c l h" . linewise-toggle-comment-out)
                       ("C-c l v" . linewise-eval)
                       ("C-c l r" . linewise-repeat)
                       ("C-c l n" . linewise-narrow)
                       ("C-c l RET" . linewise-newline)
                       ("C-c l TAB" . linewise-indent)
                       ("C-c l o" . linewise-copy-other-window)))
      (should (eq (cdr (assoc (kbd (car binding)) calls)) (cdr binding))))))

(provide 'linewise-test)

;;; linewise-test.el ends here
