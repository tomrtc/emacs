;;; init.el --- emacs configuration elisp code using req-package
;; -*- mode: emacs-lisp; auto-compile-lisp: nil; -*-

;; Copyright (C) 1996-2020 Remy TOMASETTO

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
      '((font . "Cascadia Code PL" )
         (top . 8)
         (left . 6)
         (width . 100)
         (height . 38)
        (auto-raise . t)
        (cursor-color . "orange")
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
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(setq inhibit-startup-message t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;;(menu-bar-mode -1)
(when window-system
  (set-scroll-bar-mode nil))

(blink-cursor-mode -1)
;;(set-fringe-mode 0)
;(hl-line-mode -1)
;; highlight the current line
(global-hl-line-mode +1)
;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(use-package ligature
  :load-path "~/.emacs.d/elisp/modes"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  ;;(global-ligature-mode t)
  )



(setq mouse-yank-at-point t		; Yank where the point currently is
      select-enable-primary t         ; Yank use the primary selection if available
      select-enable-clipboard t       ; Yank use the clipboard if available
      save-interprogram-paste-before-kill t ; Put clipboard/selection into kill ring
      x-selection-timeout 10                ; Workaround. See https://debbugs.gnu.org/16737
      echo-keystrokes 0.1               ; Show keystrokes early
      mouse-1-click-follows-link nil) ; Don't follow links with left click

;; Set customization data in a specific file, without littering
;; my init files.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Let me switch windows with shift-arrows instead of "C-x o" all the time
(windmove-default-keybindings)



(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq-default abbrev-mode t))

(use-package super-save
  :ensure t
  :config
  (setq auto-save-default nil)		  ; switch off the built-in auto-save-mode
  (setq super-save-auto-save-when-idle t) ;;auto-saving buffers when Emacs is idle
  (super-save-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package visual-regexp
  :bind (("<f11>"  . vr/replace)
	 ("C-<f11>"  . vr/query-replace)))

(use-package eldoc-cmake
  :hook (cmake-mode . eldoc-cmake-enable))


(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
	 ("M-o" . crux-smart-open-line)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c f" . crux-recentf-find-file)
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
	 ("s-r" . crux-recentf-find-file)
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


(use-package expand-region
  :bind
  ("C-à" . er/expand-region)
  ("C-ç" . (lambda () (interactive) (er/expand-region -1)))
  ("C-)" . (lambda () (interactive) (er/expand-region 0))))


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


(if (string-equal system-type  "gnu/linux")
    ;; aspell setup :  @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
    (use-package flyspell
      :config
      (setq flyspell-issue-message-flag nil)          ; Avoid slowdown on full buffer check.
      (setq ispell-program-name "aspell"              ; use aspell instead of ispell
	    ispell-extra-args '("--sug-mode=ultra"))
      (setq ispell-dictionary "en_US") ;; set the default dictionary
      (setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")
      (setq ispell-silently-savep t)
      (global-set-key (kbd "<f8>") 'ispell-word)
      (global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
      (defun flyspell-check-next-highlighted-word ()
	"Custom function to spell check next highlighted word."
	(interactive) (flyspell-goto-next-error) (ispell-word))
      (global-set-key (kbd "M-<f8>") #'flyspell-check-next-highlighted-word)
      (add-hook 'text-mode-hook #'flyspell-mode)
      (add-hook 'prog-mode-hook #'flyspell-prog-mode))
  ;; else windows
  (use-package flyspell
    :defer t
    :bind
    (("<f8>" . ispell-word)
     ("C-<f8>" . flyspell-check-previous-highlighted-word)
     ("M-<f8>" . flyspell-check-next-highlighted-word))
    :init
    (setenv "DICTIONARY" "en_US")
    (setq ispell-program-name "c:\\ProgramData\\chocolatey\\bin\\hunspell.exe"

	  ;; Save dictionary without asking
	  ispell-silently-savep t
	  ;; Do not issue warnings for all wrong words
	  flyspell-issue-message-flag nil)

    (defun flyspell-check-next-highlighted-word ()
      "Custom function to spell check next highlighted word"
      (interactive)
      (flyspell-goto-next-error)
      (ispell-word)
      )

    :config
    (ispell-change-dictionary "en_US" t)

    )) ; use-package flyspell


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


;; learn keyboard shortcuts.
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))


;; query-replace with feedback in modeline.
;; anzu-replace-at-cursor-thing is useful in refactoring such as changing variable name in the function.
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

;; iedit changing variable name in the function.
(use-package iedit
    :ensure t
    :defer t
    :init
    (define-key global-map (kbd "C-<") 'iedit-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))


(use-package elf-mode
  :ensure t
  :config
  (elf-setup-default))

(use-package typo
  :config
  (setq typo-language "French")
  ;; typo-mode and typo-global-mode are usable, typo-global-mode provides a prefix map
  ;; under C-c 8 which complements the default C-x 8 prefix map.
  (add-hook 'text-mode-hook 'typo-global-mode)
  :ensure t)


(use-package writegood-mode
  :ensure t)


(use-package cmake-mode
    :mode "\\(CMakeLists\\.txt\\|\\.cmake\\)\\'"
    :defer t
    :ensure t)

(use-package cmake-font-lock
    :defer t
    :config
    (add-hook 'cmake-mode-hook #'cmake-font-lock-activate)
    :ensure t)

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (setq centaur-tabs-style "bar"
;; 	centaur-tabs-height 32
;; 	centaur-tabs-set-modified-marker t
;; 	centaur-tabs-show-navigation-buttons t
;; 	centaur-tabs-set-bar 'over
;; 	)
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(use-package cmake-ide
    :defer t
    :config
    (setq cmake-ide-build-pool-dir "~/ws/productions")
    (setq cmake-ide-build-pool-use-persistent-naming t)
    (setq cmake-ide-flags-c++
	  (append '("-std=c++14""-I/usr/include/c++/6"
		    "-I/usr/include/x86_64-linux-gnu/c++/6"
		    "-I/usr/include/c++/6/backward"
		    "-I/usr/lib/gcc/x86_64-linux-gnu/6/include"
		    "-I/usr/local/include"
		    "-I/usr/lib/gcc/x86_64-linux-gnu/6/include-fixed"
		    "-I/usr/include/x86_64-linux-gnu"
		    "-I/usr/include")))
    (setq cmake-ide-flags-c '("-I/usr/include"))
    :ensure t)


(use-package graphviz-dot-mode
    :mode "\\.dot\\'"
    :ensure t
    :defer t)

(use-package asn1-mode
  :mode "\\.asn\\'"
  :ensure t
  :defer t)

(use-package compilation-mode
  ;; built-in
  :defer t
  :init (setq compilation-scroll-output 'next-error))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


(use-package aggressive-indent
  :ensure t
  :config
  ;(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'c++-mode-hook        #'aggressive-indent-mode))

(use-package pabbrev
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'pabbrev-mode))

(use-package popup
    :ensure t
    :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\(\\.md\\|\\.md\\.html\\)\\'")

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



(setq load-path (append (directory-files "~/.emacs.d/elisp" t "^[^.]")
			load-path))
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))


;; (if (string-equal system-type  "gnu/linux")
;;     (setq load-path (append (directory-files "/usr/local/share/emacs/site-lisp" t "^[^.]")
;; 			    load-path)))



(use-package re-builder
  :defer
  :config (setq reb-re-syntax 'rx))

(use-package smex
  :ensure t
  :bind (("M-x"   . smex)
         ("M-X"   . smex-major-mode-commands)
	 ("C-x m" . smex)))
(require 'keyboard)

;; ;; Line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format (quote "%4d  "))



;; Remember from session to session all the commands introduced
;; in the minibuffer, files opened, etc.
(setq savehist-file (expand-file-name "history.dot" "~/dicts"))
(savehist-mode 1)

(message ".emacs loaded")
(switch-to-buffer "*Messages*")

(load-theme 'salm t)

(provide 'init)
;;; init.el ends here
