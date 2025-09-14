;;; linewise.el --- Set of functions for linewise editing

;;; Commentary:
;;
;;
;; Copyright (C) 2017 by Sergey Kitov
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;;

;; Author: Sergey Kitov
;; URL: http://github.com/skitov/linewise
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;;
;; All functions of the package deal with "selected lines".
;; The definition of selected lines:
;; - If (and transient-mark-mode mark-active), region, extended to include all the lines intersecting with region, including trailing linefeed.
;; - Otherwise, line containing point including trailing linefeed.
;;
;; Get the code:
;;
;;   git clone https://github.com/skitov/linewise
;;
;; and add the following to your Emacs config:
;;
;;   (add-to-list 'load-path "~/path/to/linwise")
;;   (require 'linewise)
;;   If you want to add suggested keybinding add:
;;   (linewise-set-hotkeys "prefix")
;;   or:
;;   (linewise-set-hotkeys t) ;; prefix will be "C-l", original "C-l" will be unset
;;   "<M-N>" 'linewise-move-up-or-down      - moves selected lines by number of lines specified by prefix arg
;;   "<M-P>" 'linewise-move-up                - moves selected lines one line up
;;   "prefix C-n" 'linewise-move-down-fast  - moves selected lines 10 lines down
;;   "prefix C-p" 'linewise-move-up-fast      - moves selected lines 10 lines up
;;   "prefix k" 'linewise-kill                   - kill selected lines
;;   "prefix d" 'linewise-delete                 - delete selected lines without adding to kill ring
;;   "prefix c" 'linewise-copy                   - put selected lines to kill ring without deleting
;;   "prefix y" 'linewise-yank                   - yank at the beginning of current line (yanks previosly killed lines before current)
;;   "prefix h" 'linewise-toggle-comment-out     - comment/uncomment selected lines
;;   "prefix v" 'linewise-eval                   - evaluate selected lines
;;   "prefix r" 'linewise-repeat                 - fork selected lines
;;   "prefix n" 'linewise-narrow                 - narrow to selected lines
;;   "prefix RET" 'linewise-newline              - insert new line above the current line, put point to the new line and indent.
;;   "prefix TAB" 'linewise-indent               - indent selected lines
;;   "prefix o" 'linewise-copy-other-window      - copy selected lines to other window and stay there
;;   "prefix C-o" #'(lambda() (interactive) (linewise-copy-other-window t)) - copy selected lines to other window and return back to initial window
;;




;;; Code:

(defun linewise-count-lines-region-with-empty-last ()
  "Line-count in region, including empty last line."
  (if (and transient-mark-mode mark-active)
	  (let (l)
		(setq l (count-lines (region-beginning) (region-end)))
		   (if (or (= (region-beginning) (region-end))
				   (= ?\n (char-before (region-end))))
			   (1+ l)
			 (eval 'l)))
	1))

(defun linewise-affected-lines-bounds()
  "Return bounds of affected lines.
bounds are list of two elements,
where first element is beginning of affected lines,
second is end of affected lines."
  (let ((lcount (linewise-count-lines-region-with-empty-last)))
	(if (and (mark) (< (mark) (point)))
		(cons (line-beginning-position (- 2 lcount)) (line-beginning-position 2))
	  (cons (line-beginning-position) (line-beginning-position (1+ lcount))))))

(defun linewise-call-region-function(region-func)
  "Call REGION-FUNC function with bounds of affected lines as arguments."
  (let ((bounds (linewise-affected-lines-bounds)))
	(funcall region-func (car bounds) (cdr bounds))))

(defun linewise-insert-select(str)
  "Insert STR into buffer, and set insertion selected.
Trailing linefeed is excluded from selection since it would add extra line."
  (let ((l (length str)))
	(if (= (aref str (- l 1)) ?\n)
		(setq l (- l 1))
	  (setq str (concat str "\n")))
	(insert str)
	(goto-char (- (point) 1))
	(when (> (linewise-count-lines-region-with-empty-last) 1)
	  (setq deactivate-mark nil)
	  (push-mark (- (point) l) nil t))))

(defun linewise-affected-lines-content()
  "Return substring between affected lines bounds."
  (linewise-call-region-function 'buffer-substring-no-properties))

(defun linewise-delete()
  "Delete affected lines without putting to kill ring."
  (interactive)
  (linewise-call-region-function 'delete-region))

(defun linewise-copy()
  "Put affected lines to kill ring without deleting."
  (interactive)
	(linewise-call-region-function 'kill-ring-save)
	(message "Current line(s) copied to kill ring"))

(defun linewise-kill()
  "Kill affected lines."
  (interactive)
  (linewise-call-region-function 'kill-region))

(defun linewise-yank()
  "Yank to beginning of the current line."
  (interactive)
  (move-beginning-of-line 1)
  (yank))

(defun linewise-repeat(&optional arg)
  "Repeat affected lines.  Make ARG copies If ARG is provided.
Please do not use for code duplication!"
  (interactive "p")
  (let ((bounds (linewise-affected-lines-bounds))
		(start)
		(finish)
		(substr))
	(when (or (not arg) (< arg 1)) (setq arg 1))
	(setq start (car bounds))
	(setq finish (cdr bounds))
	(setq substr (buffer-substring start finish))
	(goto-char finish)
	(when (not (= ?\n (char-before finish)))
	  (insert "\n"))
	(while (> arg 0)
	  (linewise-insert-select substr)
	  (setq arg (- arg 1))
	  (when (> arg 0)
		(right-char)))))

(defun linewise-narrow()
  "Narrow buffer to selected lines."
  (interactive)
  (linewise-call-region-function 'narrow-to-region))

(defun linewise-newline()
  "Insert new line above the current line."
  (interactive)
  (goto-char (line-beginning-position))
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(defun linewise-copy-other-window(&optional keep-window)
  "Copy affected lines to other window.
If KEEP-WINDOW is nil, stays in other window.
If KEEP-WINDOW is t stays in the same window and lines
are inserted without selection, so consequent usage of
the command doesn't mix lines.  Please do not use for code duplication!"
  (interactive)
  (let ((content (linewise-affected-lines-content)))
	(other-window 1)
	(goto-char (line-beginning-position))
	(if keep-window
		;; When keeping window, next copying of lines should after these lines, but not in between.
		(progn (insert content) (other-window -1))
	  (linewise-insert-select content))))

(defun linewise-move-up-or-down(arg)
  "Shift lines affected by selection ARG lines down.
Non-positive argument moves lines up.
If selection is within one line, selection is discarded,
otherwise moved lines stay selected from beginning to end.
0 argument is 1 line up."
  (interactive "p")
  (defvar substr-beginning)
  (defvar substr-end)
  (defvar substr-bounds)
  (defvar sub-str)
  (setq substr-bounds (linewise-affected-lines-bounds))
  (setq substr-beginning (car substr-bounds))
  (setq substr-end (cdr substr-bounds))
  (defvar move-possible)
  (save-excursion
	(if (> arg 0)
		(progn (goto-char substr-end)
			   (setq move-possible (and (< (point) (point-max))
										(= (count-lines (point) (line-beginning-position (1+ arg))) arg))))
	  (progn (goto-char substr-beginning)
			 (setq move-possible (= (count-lines (line-beginning-position arg) (point)) (- 1 arg))))))

  ;; Don't move if we are already at bounds of the buffer.
  (when move-possible
	(setq sub-str (buffer-substring substr-beginning substr-end))
	(delete-region substr-beginning substr-end)
	(move-beginning-of-line (if (> arg 0) (1+ arg) arg))
	(linewise-insert-select sub-str)))

(defun linewise-move-up()
  "Call `linewise-move-up-or-down' with 0 argument."
  (interactive)
  (linewise-move-up-or-down 0))

(defun linewise-move-up-fast()
  "Call `linewise-move-up-or-down' with -9 argument."
  (interactive)
  (linewise-move-up-or-down -9))

(defun linewise-move-down-fast()
  "Call `linewise-move-up-or-down' with 10 argument."
  (interactive)
  (linewise-move-up-or-down 10))

(defun linewise-toggle-comment-out()
  "Comment/uncomment affected lines."
  (interactive)
  (linewise-call-region-function 'comment-or-uncomment-region))

(defun linewise-eval()
  "Evaluate affected lines."
  (interactive)
  (linewise-call-region-function 'eval-region))

(defun linewise-indent()
  "Indent affected lines."
  (interactive)
  (linewise-call-region-function 'indent-region))

(defun linewise-set-hotkeys(prefix)
  "Set hotkeys for the package functions.
Hotkeys will start with PREFIX,
except for moves of affected lines 1 line up or down.
Assigned keys will be:
- `M-N' `linewise-move-up-or-down'
- `M-P' `linewise-move-up'
- PREFIX `C-n' `linewise-move-down-fast'
- PREFIX `C-p' `linewise-move-up-fast'
- PREFIX `k' `linewise-kill'
- PREFIX `d' `linewise-delete'
- PREFIX `c' `linewise-copy'
- PREFIX `y' `linewise-yank'
- PREFIX `h' `linewise-toggle-comment-out'
- PREFIX `v' `linewise-eval'
- PREFIX `r' `linewise-repeat'
- PREFIX `n' `linewise-narrow'
- PREFIX `RET' `linewise-newline'
- PREFIX `TAB' `linewise-indent'
- PREFIX `o' `linewise-copy-other-window'
- PREFIX `C-o' `linewise-copy-other-window' with t as argument,
 meaning staying in the
  same window"
  (global-set-key (kbd "M-N") 'linewise-move-up-or-down)
  (global-set-key (kbd "M-P") 'linewise-move-up)
  (global-set-key (kbd (concat prefix " C-n")) 'linewise-move-down-fast)
  (global-set-key (kbd (concat prefix " C-p")) 'linewise-move-up-fast)
  (global-set-key (kbd (concat prefix " k")) 'linewise-kill)
  (global-set-key (kbd (concat prefix " d")) 'linewise-delete)
  (global-set-key (kbd (concat prefix " c")) 'linewise-copy)
  (global-set-key (kbd (concat prefix " y")) 'linewise-yank)
  (global-set-key (kbd (concat prefix " h")) 'linewise-toggle-comment-out)
  (global-set-key (kbd (concat prefix " v")) 'linewise-eval)
  (global-set-key (kbd (concat prefix " r")) 'linewise-repeat)
  (global-set-key (kbd (concat prefix " n")) 'linewise-narrow)
  (global-set-key (kbd (concat prefix " RET")) 'linewise-newline)
  (global-set-key (kbd (concat prefix " TAB")) 'linewise-indent)
  (global-set-key (kbd (concat prefix " o")) 'linewise-copy-other-window)
  (global-set-key (kbd (concat prefix " C-o")) #'(lambda() (interactive) (linewise-copy-other-window t))))

(provide 'linewise)

;;; linewise.el ends here
