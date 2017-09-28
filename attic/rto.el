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
;; Melpa: [M-x] package-install [RET] modern-cpp-font-lock [RET]
;; In your init Emacs file add:
;;     (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
;; http://stackoverflow.com/a/145359/516188




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
(defun increment-number-at-point (arg)
  (interactive "p")
  (let ((inc (or arg 1)))
    (skip-chars-backward "0123456789xABCDEFabcdef")
    (cond ((looking-at "0x\\([0123456789ABCDEFabcdef]+\\)")
           (replace-match (format "0x%x" (+ inc (string-to-number (match-string 1) 16)))))
          ((looking-at "[0123456789]+")
           (replace-match (number-to-string (+ inc (string-to-number (match-string 0))))))
(error "No number at point"))))
