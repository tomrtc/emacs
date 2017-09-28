;;; KEYBOARD.EL --- map function keys to elisp funs.

;; Copyright (C) 1996 Remy TOMASETTO

;; Author: Remy TOMASETTO <remy.tomasetto@al-enterprise.com>
;; Maintainer: Remy TOMASETTO <Remy.TOMASETTO@sxb.bsf.alcatel.fr>
;; Created: 15 Nov 1996
;; Last modification: %G%
;; Version: %I%
;; Keywords:


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <Remy.TOMASETTO@sxb.bsf.alcatel.fr>) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Change log:


;;; Code:

(defconst keyboard-version "2017.1"
  "Keyboard-version-id.")

;; Use better names than plop<1> and plop<2> for files with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-strip-common-suffix nil)


;; Goto matching parenthesis
(defun match-paren ()
  "Will bounce between matching parens just like % in vi."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
	  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
	  (t (error "%s" "Not on a paren, brace, or bracket")))))

(global-set-key [(control =)] 'match-paren)

;; ...never switch to overwrite mode, not even accidentally
(global-set-key [insert] 'undefined)




(global-set-key [f1]  'goto-line)
(global-set-key [S-f1]  'what-line)
(global-set-key [M-f1]  'mark-c-function)
(global-set-key [f2]  'delete-other-windows)
(global-set-key [f3]  'compile)
(global-set-key [f4]  'next-error)
(global-set-key [f5]  'previous-error)
(global-set-key [f6]  'vc-toggle-read-only)


(global-set-key [C-right] 'jde-javadoc-autodoc-at-line)
(global-set-key [C-left] 'jde-complete-at-point)
(global-set-key [C-up] 'senator-previous-token)
(global-set-key [C-down] 'senator-next-token)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)


(defun flush-blank-lines (start end)
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

(defun collapse-blank-lines (start end)
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))

(defun buffer-list-to-columns (ncolumns)
  "Modify a 1-column stringlist in current buffer into NCOLUMNS aligned columns.
List of items must begin in buffer column 0, and must not contain
whitespaces."
  (interactive)
  (let ((cols (1- ncolumns)) result)
    (save-excursion
      (goto-char (point-min))
      (dotimes (i cols)
        (join-line 1))
      (while (= 0 (forward-line 1))
        (dotimes (i cols)
          (join-line 1)))
      ;; for N columns, the args are:
      ;; regexp align whitespace followed by any word character
      ;; group=1, spacing=nil, repeat for the whole line=t
      (align-regexp (point-min) (point-max) "\\(\\s-*\\) \\w" 1 nil t)
      (setq result (buffer-string)))
    result))


(defun toto ()
  (interactive)
  (buffer-list-to-columns 6))





(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
     Move point to the first non-whitespace character on this line.
     If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)


(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'comment-dwim)

;; fill comment to width
(defun fill-comment ()
  "Fill text to column width for comments"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (while (< (current-column) fill-column) (insert ?#))))
(global-set-key (kbd "s-!") 'fill-comment)

;;######################################################################
(defun set-window-width (n)
  "Set the selected window's width to N columns wide."
  (if (> n (window-width))
      (enlarge-window-horizontally (- n (window-width)))
    (shrink-window-horizontally (- (window-width) n))))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

;;; Parenthesis

;; Use mic-paren in replacement of standard paren.el
(use-package mic-paren
  :ensure t
  :config
  (paren-activate)                      ; activating
  (add-hook 'c-mode-common-hook
	    (function (lambda ()
			(paren-toggle-open-paren-context 1))))
  ;; In LaTeX-mode we want this
  (add-hook 'LaTeX-mode-hook
	    (function (lambda ()
			(paren-toggle-matching-quoted-paren 1)
			(paren-toggle-matching-paired-delimiter 1)))))



(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'")


(provide 'keyboard)
;;; KEYBOARD.EL ends here
