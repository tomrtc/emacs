;;; init.el --- emacs configuration elisp code using req-package
;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-

;; Copyright (C) 2017 Remy TOMASETTO

;; Author: Remy TOMASETTO <remy.tomasetto@al-enterprise.com>
;; Keywords:
;;; Commentary:

;;; Change log:


;;; Code:
;; (toggle-debug-on-error)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(eval-when-compile (package-initialize))

(unless (package-installed-p 'auto-compile)
  (package-refresh-contents)
  (package-install 'auto-compile))
(eval-when-compile
  (require 'auto-compile))

;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; use package
(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))
(eval-when-compile
  (require 'req-package))


;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq default-frame-alist
      '((font . "Knack Nerd Font" )
         (top . 8)
         (left . 6)
         (width . 100)
         (height . 38)
        (auto-raise . t)
        (cursor-color . "red")
        ))


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq line-number-display-limit nil)
(setq line-number-display-limit-width 250000)
;; Newline at end of file
(load "files")
(defvar require-final-newline)
(setq require-final-newline t)
(defvar user-mail-address)
(setq user-full-name "Rémy Tomasetto"
      user-mail-address "remy.tomasetto@al-enterprise.com")
(setq message-alternative-emails (rx "remy.tomasetto@gmail.com"))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)


(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(when window-system
  (set-scroll-bar-mode nil))

(blink-cursor-mode -1)
(set-fringe-mode 0)
;(hl-line-mode -1)
;; highlight the current line
(global-hl-line-mode +1)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)


(setq mouse-yank-at-point t		; Yank where the point currently is
      x-select-enable-primary t         ; Yank use the primary selection if available
      x-select-enable-clipboard t       ; Yank use the clipboard if available
      save-interprogram-paste-before-kill t ; Put clipboard/selection into kill ring
      x-selection-timeout 10                ; Workaround. See https://debbugs.gnu.org/16737
      echo-keystrokes 0.1               ; Show keystrokes early
      mouse-1-click-follows-link nil) ; Don't follow links with left click

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
(setq-default abbrev-mode t))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package abbrev
  :config
  (defvar save-abbrevs)
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
	 ("M-o" . crux-smart-open-line)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c f" . crux-recentf-ido-find-file)
	 ("C-M-z" . crux-indent-defun)
	 ("C-c u" . crux-view-url)
	 ("C-c e" . crux-eval-and-replace)
	 ("C-c w" . crux-swap-windows)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c r" . crux-rename-buffer-and-file)
	 ("C-c t" . crux-visit-term-buffer)
	 ("C-c C-k" . crux-kill-other-buffers)
	 ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)
	 ("s-r" . crux-recentf-ido-find-file)
	 ("s-j" . crux-top-join-line)
	 ("C-^" . crux-top-join-line)
	 ("s-k" . crux-kill-whole-line)
	 ("C-<backspace>" . crux-kill-line-backwards)
	 ("s-o" . crux-smart-open-line-above)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ([(shift return)] . crux-smart-open-line)
	 ([(control shift return)] . crux-smart-open-line-above)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (defvar whitespace-line-column)
  (defvar whitespace-style)
  (setq whitespace-line-column 120) ;; limit line length
  (setq whitespace-style
	'(face  empty trailing lines-tail empty )))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))



(use-package elf-mode
  :ensure t
  :config
  (elf-setup-default))

(use-package cmake-mode
    :mode "\\(CMakeLists\\.txt\\|\\.cmake\\)\\'"
    :defer t
    :ensure t)

(use-package graphviz-dot-mode
    :mode "\\.dot\\'"
    :ensure t
    :defer t)

(use-package asn1-mode
    :mode "\\.asn\\'"
    :ensure t
    :defer t)

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package pabbrev
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'pabbrev-mode))

(require 'popup)

(defun pabbrevx-suggestions-goto-buffer (suggestions)
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






(setq load-path (append (directory-files "~/.emacs.d/elisp" t "^[^.]")
			load-path))

(defun chargeur (filename)
  "Def: chargeur : french loading the FILENAME."
  (let ((file (expand-file-name filename)))
    (if (file-exists-p file)
	(load-file file))))

(require 'defaults)
(require 'keyboard)
(require  'cpp-auto-include)


;;(require 'asn1-mode)
(require 'txl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?\\)$" . txl-mode))

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
    (progn
     (delete-windows-on buffer)
     (tooltip-show "\n Compilation Successful :-) \n "))
    (tooltip-show "\n Compilation Failed :-( \n "))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

(global-set-key (kbd "<f3>") 'compile-again)

(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  "Run the same compile with PFX as the last time.
If there was no last time, or there is a prefix argument, this acts like compile."
 (interactive "p")
 (if (and (eq pfx 1)
	  compilation-last-buffer)
     (progn
       (set-buffer compilation-last-buffer)
       (revert-buffer t t))
   (call-interactively 'compile)))

(message ".emacs loaded")
(switch-to-buffer "*Messages*")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode mic-paren modern-cpp-font-lock asn1-mode graphviz-dot-mode cmake-mode elf-mode move-text anzu which-key flycheck crux magit req-package use-package auto-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
