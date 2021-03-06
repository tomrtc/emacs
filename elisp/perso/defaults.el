;;; DEFAULT.EL --- map function keys to elisp funs.

;; Copyright (C) 2018 Remy TOMASETTO

;; Author: Remy TOMASETTO <remy.tomasetto@al-enterprise.com>
;; Maintainer: Remy TOMASETTO <Remy.TOMASETTO@sxb.bsf.alcatel.fr>
;; Created: 15 Nov 1996
;; Last modification: Fri 05 Oct 2018 03:15:46 PM CEST
;; Version: 2018.1
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



(setq-default ;; dired.c
 completion-ignored-extensions
 '(".a"  ".bin"  ".elc" ".git/" ".hg/" ".map" ".mem"  ".pyc"  ".so"
   "CVS/"  "~"".o" "toto" ))

;; Use better names than plop<1> and plop<2> for files with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-strip-common-suffix nil)



(setq-default ;; minibuf.c
 ;; NOTE enable-recursive-minibuffers this can be quite confusing
 enable-recursive-minibuffers nil)


(defun pabbrevx-suggestions-goto-buffer (suggestions)
  "Essay abbrev with popup menu on SUGGESTIONS."
  (let* ((candidates (mapcar 'car suggestions))
         (bounds (pabbrev-bounds-of-thing-at-point))
         (selection (popup-menu* candidates
                                 :point (car bounds)
                                 :scroll-bar t)))
    (when selection
      ;; modified version of pabbrev-suggestions-insert
      (let ((point))
        (save-excursion
          (progn
            (delete-region (car bounds) (cdr bounds))
            (insert selection)
            (setq point (point))))
        (if point
            (goto-char point))
        ;; need to nil this so pabbrev-expand-maybe-full won't try
        ;; pabbrev expansion if user hits another TAB after ac aborts
        (setq pabbrev-last-expansion-suggestions nil)
        ))))

(fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer)


;; `describe-bindings' orders the keymaps by precedence so the
;; major-mode goes next to last, which makes it a bit inconvenient
;; for quick lookup.
(defun describe-major-mode-bindings ()
  (interactive)
  (call-interactively 'describe-bindings)
  (with-current-buffer (help-buffer)
    (search-forward "Major Mode Bindings")
    (narrow-to-page)))

;; ;; Line numbers
;; (require 'linum)
;; (global-linum-mode 1)
;; (setq linum-format (quote "%4d  "))




;; Function for finding out info about font at cursor
(defun what-face (pos)
  "Describe what face is at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(use-package powerline
  :ensure t)

(setq ns-use-srgb-colorspace nil) ;; DON'T MESS UP THE COLORS
(defun powerline-uber-theme ()
  "uber"
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw " %* " nil 'l)
				     (powerline-buffer-id nil 'l)
				     (powerline-raw " ")
				     (funcall separator-left mode-line face1)
				     (powerline-narrow face1 'l)
				     (powerline-vc face1)

				     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face2 'l))
                                     (powerline-major-mode face2 'l)
                                     (powerline-process face2)
                                     (powerline-raw " " face2)
                                     (funcall separator-left face2 face1)))

			  (rhs (list (powerline-raw global-mode-string face1 'r)
				     (powerline-raw "%4l" face1 'r)
				     (powerline-raw ":" face1)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw "    ")
				     (powerline-raw "%8p" nil 'r)))
			  (center (list )))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs))))))

  (if (eq system-type 'darwin)
      (setq powerline-text-height-hack 170)
    (setq powerline-text-height-hack 130))


  (set-face-attribute 'mode-line nil
                      :foreground "#FFF" ;; #0BF
                      :background "#234"
                      :box nil
                      :weight 'normal
                      :height powerline-text-height-hack
                      )

  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "white"
                      :weight 'normal
                      )

  (set-face-attribute 'powerline-active1 nil
                      :foreground "gray50"
                      :background "#ccc")

  (set-face-attribute 'powerline-active2 nil
                      :foreground "white"
                      :background "#567" ;; 00BF8F
                      :weight 'normal)

  (set-face-attribute 'mode-line-inactive nil
                      :foreground "gray90"
                      :background "gray65"
                      :box nil
                      :weight 'bold)
  (set-face-attribute 'powerline-inactive1 nil
                      :foreground "#aaa"
                      :background "#ddd")
  (set-face-attribute 'powerline-inactive2 nil
                      :foreground "#ccc"
                      :background "#eee")
  )

(powerline-uber-theme)

(setq powerline-height 24)




(require 'volatile-highlights)
(volatile-highlights-mode t)
(require  'cpp-auto-include)


(require 'txl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?\\)$" . txl-mode))



;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)


; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(set-face-background 'hl-line "#372E2D")

(require 'memento-mori)
(memento-mori-mode)



(require 'xterm-color)

(use-package shell-pop
  :defer t
  :custom
  ;; This binding toggles popping up a shell, or moving cursour to the shell pop-up.
  (shell-pop-universal-key "C-t")
  ;; Percentage for shell-buffer window size.
  (shell-pop-window-size 30)
  ;; Position of the popped buffer: top, bottom, left, right, full.
  (shell-pop-window-position "bottom")
  ;; Please use an awesome shell.
  (shell-pop-term-shell "/bin/zsh"))

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; Also set TERM accordingly (xterm-256color) in the shell itself.

;; An example configuration for eshell

(require 'eshell) ; or use with-eval-after-load

(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
(setenv "TERM" "xterm-256color")

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)





(provide 'defaults)
;;; defaults.el ends here
