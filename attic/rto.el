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
