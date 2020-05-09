;;; CCC.EL --- customize c++ Emacs facilities.

;; Copyright (C) 2020 Remy TOMASETTO

;; Author: Remy TOMASETTO <remy.tomasetto@al-enterprise.com>


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;;; Change log:

;;; Code:

(defconst ccc-version "2020.5"
  "The ccc-version-id.")

(require 'cc-mode)

(define-skeleton insert-namespace
  "Insert namespace."
  "Namespace: "
  > "namespace " str & " " "{" \n
  > _ \n
  "} // namespace " str | -10 & "anonymous namespace" > \n)

(define-abbrev c++-mode-abbrev-table "namespace" "" 'insert-namespace :system t)

(define-skeleton insert-stream-cout
  ""
  nil
  \n >
  "std::cout << " _ " << std::endl;"
  \n >)

(define-abbrev c++-mode-abbrev-table "cout"  "" 'insert-stream-cout)

(defun ska-skel-cc-class (name)
  "Insert a C++ class NAME definition.
It creates a matching header file, inserts the class definition and
creates the  most important function templates in a named after the
class name."
  (interactive "sclass name: ")
  (let* ((header-file-name (concat name ".hh"))
         (header-include-string (upcase (concat name "_HH_INCLUDED")))
         (def-file-name    (concat name ".cc")))

    ;; write header file
    (set-buffer (get-buffer-create header-file-name))
    (set-visited-file-name header-file-name)
    (c++-mode)
    (turn-on-font-lock)
    (insert (concat
             "// -*- C++ -*-\n"
             "// File: " header-file-name "\n//\n"
             "// Time-stamp: <>\n"
             "// $Id: $\n//\n"
             "// Copyright (C) "(substring (current-time-string) -4)
             " by " (user-full-name)  "\n//\n"
             "// Author: " (user-full-name) "\n//\n"
             "// Description: \n// "
             ;; get this point...
             "\n\n"
             "# ifndef " header-include-string "\n"
             "# define " header-include-string "\n\n"
             "# include <stdio.h>\n\n"
             "# include <stdlib.h>\n\n"
             "# include <string>\n"
             "# include <vector>\n\n"
             "# include <mtrandom>\n\n"
             "class " name ";\n\n"
             "class " name " {\n"
             "public:\n"
             name "();" "\n"
             name "(const " name "& src);\n"
             "~" name "();" "\n"
             name "& operator=(const " name "& rv);\n"
             "\nprivate:\n"
             "void init();\n"
             "void reset();\n"
             "void init_and_copy(const " name "& src);\n\n"
             "protected: \n\n"
             "};"
             "\n\n# endif"))
    (beginning-of-buffer)
    (while (and (not (eobp)) (forward-line))
      (indent-according-to-mode))

    ;; create CC file
    (set-buffer (get-buffer-create def-file-name))
    (set-visited-file-name def-file-name)
    (switch-to-buffer (current-buffer))
    (c++-mode)
    (turn-on-font-lock)
    (insert (concat
             "// -*- C++ -*-\n"
             "// File: " def-file-name "\n//\n"
             "// Time-stamp: <>\n"
             "// $Id: $\n//\n"
             "// Copyright (C) "(substring (current-time-string) -4)
             " by " (user-full-name) "\n//\n"
             "// Author: " (user-full-name) "\n//\n"
             "// Description: \n//\n\n "
             "# include <stdio.h>\n\n"
             "# include <string>\n"
             "# include <vector>\n\n"
             "# include <mtrandom>\n"
             "\n# include \"" header-file-name "\"\n\n"
             name "::\n" name "() {\ninit();\n}\n\n"
             name "::\n" name "(const " name "& src) {\ninit_and_copy(src);\n}\n\n"
             name "::\n~" name "() {\nreset();\n}\n\n"
             "void\n" name "::\ninit() {\n\n}\n\n"
             "void\n" name "::\nreset() {\n\n}\n\n"
             "void\n" name "::\ninit_and_copy(const " name "& src) {\n\n}\n\n"
             name "&\n" name "::\noperator=(const " name "& src) {\n\n}\n\n"
             ))
    (beginning-of-buffer)
    (while (and (not (eobp)) (forward-line))
      (indent-according-to-mode))
    (beginning-of-buffer)
    (search-forward "Description:")
    )
)

(define-skeleton add-tags
  "Enter tags in any case and the output will be up-cased."
  nil
  "Tags: "
  ((upcase (skeleton-read "Tag: ")) ":"  str) ":" \n)



;;; skeletons
;; -----------

(require 'semantic/bovine/gcc)

;;  c
(define-skeleton skeleton-c-mode-main-fun
  "generate main(int argc, char *argv[])"
  > "int main(int argc, char *argv[])\n{\n"
  > _ " "
  > "\n\nreturn 0;"
  > "\n}")

(define-skeleton skeleton-c-mode-main-fun1
  "generate main()"
  > "int main()\n{\n"
  > _ " "
  > "\n\nreturn 0;"
  > "\n}")

(define-skeleton skeleton-c-mode-include
  "Generate include<>."
  > "#include <"
  (completing-read
   "Include File: "
   (apply 'append
          (mapcar (lambda (dir) (directory-files dir))
                  (append (semantic-gcc-get-include-paths "c")
                          (remove-if-not 'file-exists-p '("~/include"))))
          ))
  ">\n")

;; c++
(define-skeleton skeleton-c++-mode-main-fun
  "generate int main(int argc, char *argv[])"
  > "int main(int argc, char *argv[])\n{\n"
  > _ " "
  > "\n}")

(define-skeleton skeleton-c++-mode-main-fun1
  "generate int main()"
  > "int main()\n{\n"
  > _ ""
  > "\n}")

(define-skeleton skeleton-c++-mode-include
  "Generate include<>."
  > "#include <"
  (completing-read
   "Include file: "
   (apply 'append
          (mapcar (lambda (dir) (directory-files dir))
                  (semantic-gcc-get-include-paths "c++"))
          ))
  ">\n")





(defun smart-align ()
  (interactive)
  (with-demoted-errors
      "Something wrong when align."
    (let ((align-start
           (save-excursion
             (backward-up-list)
             (point)
             ))
          (align-end
           (save-excursion
             (up-list)
             (point))))
      (align-regexp align-start align-end "\\(\\s-*\\)\\(=\\|:\\)" 1 1))))



(defun duplicate-line-or-region-above (&optional reverse)
  "Duplicate current line or region above.
By default, duplicate current line above.
If mark is activate, duplicate region lines above.
Default duplicate above, unless option REVERSE is non-nil."
  (interactive)
  (let ((origianl-column (current-column))
        duplicate-content)
    (if mark-active
        ;; If mark active.
        (let ((region-start-pos (region-beginning))
              (region-end-pos (region-end)))
          ;; Set duplicate start line position.
          (setq region-start-pos (progn
                                   (goto-char region-start-pos)
                                   (line-beginning-position)))
          ;; Set duplicate end line position.
          (setq region-end-pos (progn
                                 (goto-char region-end-pos)
                                 (line-end-position)))
          ;; Get duplicate content.
          (setq duplicate-content (buffer-substring region-start-pos region-end-pos))
          (if reverse
              ;; Go to next line after duplicate end position.
              (progn
                (goto-char region-end-pos)
                (forward-line +1))
            ;; Otherwise go to duplicate start position.
            (goto-char region-start-pos)))
      ;; Otherwise set duplicate content equal current line.
      (setq duplicate-content (buffer-substring
                               (line-beginning-position)
                               (line-end-position)))
      ;; Just move next line when `reverse' is non-nil.
      (and reverse (forward-line 1))
      ;; Move to beginning of line.
      (beginning-of-line))
    ;; Open one line.
    (open-line 1)
    ;; Insert duplicate content and revert column.
    (insert duplicate-content)
    (move-to-column origianl-column t)))

(defun duplicate-line-or-region-below ()
  "Duplicate current line or region below.
By default, duplicate current line below.
If mark is activate, duplicate region lines below."
  (interactive)
  (duplicate-line-or-region-above t))

(defun duplicate-line-above-comment (&optional reverse)
  "Duplicate current line above, and comment current line."
  (interactive)
  (if reverse
      (duplicate-line-or-region-below)
    (duplicate-line-or-region-above))
  (save-excursion
    (if reverse
        (forward-line -1)
      (forward-line +1))
    (comment-or-uncomment-region+)))

(defun duplicate-line-below-comment ()
  "Duplicate current line below, and comment current line."
  (interactive)
  (duplicate-line-above-comment t))

(defun comment-or-uncomment-region+ ()
  "This function is to comment or uncomment a line or a region."
  (interactive)
  (let (beg end)
    (if mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (setq beg (line-beginning-position))
      (setq end (line-end-position)))
    (save-excursion
      (comment-or-uncomment-region beg end))))


(provide 'ccc)
;;; CCC.EL ends here
