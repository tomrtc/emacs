;;; KEYBOARD.EL --- map function keys to elisp funs.

;; Copyright (C) 1996-2017 Remy TOMASETTO

;; Author: Remy TOMASETTO <remy.tomasetto@al-enterprise.com>


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

(defconst keyboard-version "2017.2"
  "Keyboard-version-id.")



;; unbound suspend-frame
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


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

;; ;; Load rtags and start the cmake-ide-setup process
;; (require 'rtags)

;; (setq rtags-autostart-diagnostics t)
;; (rtags-diagnostics)
;; (setq rtags-completions-enabled t)
;; (rtags-enable-standard-keybindings)
;; (cmake-ide-setup)


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
(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)


(defun open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir (locate-dominating-file (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))
(global-set-key (kbd "C-c C-f") 'open-readme-in-git-root-directory)



(defun jart-unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line.
Thanks: Stefan Monnier <foo@acm.org>"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun jart-face-at-point ()
  "Tell me who is responsible for ugly color under cursor."
  (interactive)
  (message "%S: %s" (face-at-point)
	   (face-documentation (face-at-point))))

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


(defun insert-current-time (prefix)
  "Insert the current date. With prefix-argument, use 24h format.
   With two prefix arguments, write out an ISO 8601 date and
   time."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%FT%T%z")
                 ((equal prefix '(4)) "%T")
                 ((equal prefix '(16)) "%I:%M:%S %p"))))
    (insert (format-time-string format))))

;; Insert generated UUIDs
(random t)

(defun random-ms-uuid ()
  (format "%04x%04x-%04x-4%s-%s-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (substring (format "%04x" (random (expt 16 4))) 1)
          (concat
           (let ((n (random 4)))
             (substring "89ab" n (1+ n)))
           (substring (format "%04x" (random (expt 16 4))) 1))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun random-xcode-uuid ()
  (format "%04X%04X%04X%04X%04X%04X"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(defun insert-uuid (prefix)
  "Insert a random universally unique identifier (UUID). A UUID
is a 128-bit (16 byte) number formatted in a certain way.
Example of a UUID: 1df63142-a513-X850-Y1a3-535fc3520c3d
Where X is 4 and Y is one of {8,9,a,b}.
With a prefix argument, insert a random UUID suitable for use in
XCode projects. An XCode UUID is a 96-bit (12 byte) number
formatted as a hex string.
Example of an XCode UUID: a513b85041a3535fc3520c3d."
  (interactive "P")
  (insert
   (cond
    ((not prefix) (random-ms-uuid))
    ((equal prefix '(4)) (random-xcode-uuid)))))

;;(bind-key "C-c u" 'insert-uuid)
(defun m/query-replace-using-region (start end)
  "Like `query-replace' but uses region as the search string"
  (interactive "r")
  (let ((use-region-p nil)
        (from (buffer-substring-no-properties start end)))
    (deactivate-mark)
    (goto-char start)
    (perform-replace from
                     (read-from-minibuffer (format "Replace %s with: "
                                                   from))
		     t nil nil)))

(defun convert-size-to-bytes (s)
  "Given a size with suffix K or M, returns the size in bytes"
  (let* ((slen (length s))
         (all-but-last (substring s 0 (- slen 1 )))
         (last-char (downcase (substring s (- slen 1) slen))))
    (cond
     ((string= last-char "k") (* 1024 (string-to-number all-but-last)))
     ((string= last-char "m") (* 1048576 (string-to-number all-but-last))))))

(defun strip-text-properties(txt)
  "Does what it sayd. http://stackoverflow.com/a/8377127/209050"
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun m/-extract-field-from-region (start end N delimiter)
  "Extract field `N' delimited by `delimiter' from region
specified by `start' and `end'"
  (goto-char start)
  (beginning-of-line)
  (let ((current-point (point))
        (result))
    (while (< current-point end)
      (push (nth N
                 (split-string (strip-text-properties (thing-at-point 'line))
                               delimiter))
            result)
      (forward-line)
      (setq current-point (point)))
    (reverse result)))

(defun m/mapcfield (fn N &optional delimiter)
  "Like `mapc' but evaluates `fn' for each field in the region defined by
`N' (field number) and `delimiter' (using `m/-extract-field-from-region')."
  (setq delimiter (or delimiter " "))
  (mapc fn (m/-extract-field-from-region (region-beginning)
                                         (region-end)
                                         N
                                         delimiter)))

(defun m/extract-field-from-region (start end)
  "Like cut -dD -fN where D and N are read from the user"
  (interactive "r")
  (let ((res (m/-extract-field-from-region start
                                           end
                                           (string-to-number (read-from-minibuffer "Field: "
                                                                                   "0"))
                                           (read-from-minibuffer "Field [default=\" \"]: "
                                                                 " "))))
    (message "%s" (mapconcat 'identity res "\n"))))

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

;;; from https://github.com/thomasf/dotfiles-thomasf-emacs
(defun toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1)))))
  (make-repeatable 'toggle-fold))

;;; from http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))



;; Let me write these characters, plx
(global-set-key (kbd "M-2") "@")
(global-set-key (kbd "M-4") "$")
(global-set-key (kbd "M-8") "[")
(global-set-key (kbd "M-9") "]")
(global-set-key (kbd "M-(") "{")
(global-set-key (kbd "M-)") "}")
(global-set-key (kbd "M-7") "|")
(global-set-key (kbd "M-/") "\\")
(global-set-key (kbd "C-x M-l") "λ")
(global-set-key (kbd "M-n") 'next-error) ; also works for rgrep results
(global-set-key (kbd "M-p") 'previous-error)

;; Home/End keyboard shortcuts
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
   Move point to the first non-whitespace character on this line.
   If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))


(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)


(define-key global-map [end] 'end-of-line)
(global-set-key (kbd "C-e") 'end-of-line)

(global-set-key (kbd "C-:") 'dabbrev-expand)
(global-set-key (kbd "C-!") 'ispell-word-then-abbrev)


(defun ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)

          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))











(define-generic-mode 'ragel-mode
  '(?#) ;; Comments
  '(
    ;; Keywords
    "machine" "action" "access" "context" "include" "import" "export"
    "prepush" "postpop" "when" "inwhen" "outwhen" "err" "lerr" "eof" "from"
    "to" "alphtype" "getkey" "write"
    ;; Rules
    "any" "ascii" "extend" "alpha" "digit" "alnum" "lower" "upper"
    "xdigit" "cntrl" "graph" "print" "punct" "space" "zlen" "empty"
    ;; Inline code matching
    "fpc" "fc" "fcurs" "fbuf" "fblen" "ftargs" "fstack"
    "fhold" "fgoto" "fcall" "fret" "fentry" "fnext" "fexec" "fbreak"
    )
  '(
    ;; Literals
    ;;("\\([^\\)]*\\)" . font-lock-constant-face)
    ;;("\\[[[^\\]]*\\]" . font-lock-constant-face)
    ("\(\"\\?'\"\'|\\?\"'\|'[^']*'\|\"[^\"]*\"\)" . font-lock-constant-face)
    ;; Numbers
    ("\\<[0-9][0-9]*\\>" . font-lock-constant-face)
    ("\\<0x[0-9a-fA-F][0-9a-fA-F]*\\>" . font-lock-constant-face)
    ;; Operators
    ("[>$%@]" . font-lock-constant-face)
    ("<>\|<" . font-lock-constant-face)
    ;;("[>\<$%@][!\^/*~]" . font-lock-constant-face)
    ;;("[>$%]?" . font-lock-constant-face)
    ;;("<>[!\^/*~]" . font-lock-constant-face)
    ("=>" . font-lock-constant-face)
    ("->" . font-lock-constant-face)
    (":>" . font-lock-constant-face)
    (":>>" . font-lock-constant-face)
    ("<:" . font-lock-constant-face)
    )
  nil ;'(".rl\\'")
  nil
  "Generic mode for mmm-mode editing .rl files.")

(provide 'keyboard)
;;; KEYBOARD.EL ends here
