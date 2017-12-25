;;; include.el --- Insert appropriate #includes to call library functions

;; Copyright (C) 2000 Victor Zandy

;; Author: Victor Zandy <zandy@cs.wisc.edu>
;; Created: September 24, 1998

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This program spares you from having to remember or lookup the
;; header files required to call Unix system and library functions in
;; C programs.  Instead, type "M-x include".  It will prompt for the
;; function and #include the required headers for you.

;; include.el stores a database of function/header associations in
;; `include-db-filename'.  The associations are determined by scanning
;; manpages.  By default only the section 2 manpages (system calls)
;; are scanned, but you can add others to `include-db-scan-dirs'.  A
;; separate database is maintained for each operating system you use.

;; User functions:
;;   `include'
;;      Prompt for function name and #include appropriate headers.
;;   `include-load-db'
;;      Re-read the include database from disk or create it if it does
;;      not exist.
;;   `include-scan'
;;      Create the include database.
;;
;; User variables:
;;   `include-db-scan-dirs'
;;      Directories containing manpages to scan for functions and
;;      headers.
;;   `include-db-filename'
;;      Filename for the database of include information.

;;; Bugs:

;; * `include' is not aware of header files already #included by other
;;   header files.
;; * There should be a way to limit the manpages scanned within a
;;   particular directory.

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl) ; push and pop

(defvar include-db-scan-dirs
  '("/usr/man/man2" "/usr/share/man/man2")
  "*List of directories with manpages to scan for function #includes.")

(defvar include-db-filename
  (expand-file-name (concat "~/"
			    ".emacs.incl."
			    (progn
			      (string-match "[^-]+-[^-]+-\\(.*\\)"
					    system-configuration)
			      (match-string 1 system-configuration))))
  "*Filename for the include database.")

(defvar include-db nil
  "The include database.")

(defvar include-history nil)

(defun include (syscall)
  "Insert #include's required to call SYSCALL."
  (interactive (list
		(progn
		  (or include-db (include-load-db))
		  (completing-read "Function: " include-db
				   nil nil "" 'include-history))))
  (or include-db (include-load-db))
  (let ((match (assoc syscall include-db))
	hdrs)
    (or match (error (format "I don't know about %s" syscall)))
    (setq hdrs (include-filter-included (cdr match)))
    (save-excursion
      (goto-char (include-first-include))
      (while hdrs
	(include-insert-header (pop hdrs))))))

(defun include-present-headers ()
  "Return a list of all headers #included in the current buffer."
  (let (present)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "^#include *\\([a-zA-Z0-9_<>/\\.\"]*\\) *$"
	      nil t)
	(setq present (cons (match-string 1) present))))
    present))

(defun include-filter-included (headers)
  "Remove headers names in HEADERS that are already #included in the
current buffer."
  (let ((present (include-present-headers))
	filtered)
    (while headers
      (let ((header (car headers)))
	(if (not (member header present))
	    (setq filtered (cons header filtered)))
	(setq headers (cdr headers))))
    (reverse filtered)))


(defun include-first-include ()
  "Return the position of the the first #include declaration in the
current buffer, or (point-min) if there are none."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#include" nil t)
	(progn
	  (beginning-of-line)
	  (point))
      ;; We should find a more suitable place.  Even though the
      ;; program is so simple that it doesn't include anything, the
      ;; author might have a big comment at the top for their name and
      ;; a copyright.  We shouldn't preempt it.
      (point-min))))

(defun include-insert-header (header)
  "Insert an #include for HEADER at point."
  (insert (format "#include %s\n" header)))

;;; The remaining functions manage the database.

(defun include-scan-funcname ()
  "Scan a manpage (roff source) for functions and headers.  Return a
list ((FUNC HEADER ...) ...) of functions and associated include
files."
  (let (result)
    (or (re-search-forward "SYNOPSIS" nil t)
	(signal 'file-error
		(format "%s doesn't look like a function manpage"
			(buffer-file-name))))
    (forward-line 1)
    (let (calls headers)
      (while (not (or (eobp) (looking-at "^\\.SH")))
	(cond ((looking-at "^.*\\(<[^>]+>\\).*$")
	       (push (match-string 1) headers))
	      ((looking-at "^.*[ \t]+\\([A-Za-z0-9_]+\\)(")
	       (push (match-string 1) calls)))
	(forward-line 1))
      (while calls
	(push (cons (pop calls) headers) result)))
    result))

(defun include-scan-file (file)
  "Scan manpage (roff source) FILE for functions and headers."
  (or (file-readable-p file)
      (signal 'file-error (format "%s does not exist or is unreadable" file)))
  (save-excursion
    ;; FIXME: Why does passing a nil RAWFILE arg cause the "file is
    ;; write protected" message to be displayed?
    (set-buffer (find-file-noselect file t))
    (goto-char (point-min))
    (prog1
	(condition-case nil
	    (include-scan-funcname)
	  (file-error nil))
      (kill-buffer (current-buffer)))))

(defun include-scan-dirs (dirs)
  "Scan directories containing manpages for functions and headers."
  (let (result)
    (while dirs
      (let* ((dir (pop dirs))
	     (files (and (file-exists-p dir)
			 (directory-files dir t "[^\.].*"))))
	(while files
	  (setq result (nconc (include-scan-file (pop files)) result)))))
    result))

(defun include-scan-format (s)
  "Print a function/header entry in the current buffer."
  (while s
    (let* ((e (pop s))
	   (call (car e))
	   (hdrs (cdr e)))
      (insert call " ")
      (when hdrs
	(while hdrs
	  (insert (pop hdrs) " "))
	(delete-backward-char 1))
      (insert "\n"))))

(defun include-scan ()
  "Create the include database.  The manpage is the directories listed
in `include-db-scan-dirs' are scanned and the database is saved to
`include-db-filename'."
  (interactive)
  (or (file-writable-p include-db-filename)
      (error (format "%s is not writable" include-db-filename)))
  (save-excursion
    (find-file include-db-filename)
    (erase-buffer)
    (message "Building include database...")
    (include-scan-format (include-scan-dirs include-db-scan-dirs))
    (save-buffer 0)
    (kill-buffer (current-buffer))
    (message (format "Building include database...done"))))

(defun include-load-db ()
  "Load the include database or create it (see `include-scan') if it
does not exist."
  (interactive)
  (setq include-db nil)
  (or (file-readable-p include-db-filename)
      (include-scan))
  (save-excursion
    (set-buffer (find-file-noselect include-db-filename t nil))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((str (buffer-substring (point)
				   (save-excursion
				     (end-of-line)
				     (point))))
	    call hdrs)
	(string-match "[A-Za-z0-9_]+" str)
	(setq call (match-string 0 str))
	(setq str (substring str (match-end 0) (length str)))
	;; FIXME: Don't assume headers are enclosed with <> (what about ""?)
	(while (string-match "<[^>]+>" str)
	  (let ((beg (match-beginning 0))
		(end (match-end 0)))
	    (push (match-string 0 str) hdrs)
	    (setq str (substring str end (length str)))))
	(push (cons call hdrs) include-db))
      (forward-line 1))
    (kill-buffer (current-buffer))))

(provide 'include)

;;; include.el ends here
