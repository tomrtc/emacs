;;;; package --- Summary

;;; Commentary:

;;; Code:
(deftheme salm
  "Subdued theme based on Solarized palette.")

(defgroup salm nil
  "Salm theme options.
Reload the theme after changing anything in this group."
  :group 'faces)

(defcustom
  mery-butter-1   "#fce94f" "butter 1"     :group 'salm :type '(color))
(defcustom
  mery-butter-2   "#edd400" "butter 2"     :group 'salm :type '(color))
(defcustom
  mery-butter-3   "#c4a000" "butter 3"     :group 'salm :type '(color))

(defcustom
  mery-orange-1   "#fcaf3e" "orange 1"     :group 'salm :type '(color))
(defcustom
  mery-orange-2   "#f57900" "orange 2"     :group 'salm :type '(color))
(defcustom
  mery-orange-3   "#ce5c00" "orange 3"     :group 'salm :type '(color))

(defcustom
  mery-choc-1  "#e9b96e" "choc-1"     :group 'salm :type '(color))
(defcustom
  mery-choc-2  "#c17d11" "choc-2"     :group 'salm :type '(color))
(defcustom
  mery-choc-3  "#8f5902" "choc-3"     :group 'salm :type '(color))

(defcustom
  mery-cham-1  "#8ae234" "cham-1"     :group 'salm :type '(color))
(defcustom
  mery-cham-2  "#73d216" "cham-2"     :group 'salm :type '(color))
(defcustom
  mery-cham-3  "#4e9a06" "cham-3"     :group 'salm :type '(color))

(defcustom
  mery-blue-1  "#729fcf" "blue-1"     :group 'salm :type '(color))
(defcustom
  mery-blue-2  "#3465a4" "blue-2"     :group 'salm :type '(color))
(defcustom
  mery-blue-3  "#204a87" "blue-3"     :group 'salm :type '(color))

(defcustom
  mery-plum-1  "#ad7fa8" "plum-1"     :group 'salm :type '(color))
(defcustom
  mery-plum-2  "#75507b" "plum-2"     :group 'salm :type '(color))
(defcustom
  mery-plum-3  "#5c3566" "plum-3"     :group 'salm :type '(color))

(defcustom
  mery-red-1  "#ef2929" "red-1"     :group 'salm :type '(color))
(defcustom
  mery-red-2  "#cc0000" "red-2"     :group 'salm :type '(color))
(defcustom
  mery-red-3  "#a40000" "red-3"     :group 'salm :type '(color))

(defcustom
  mery-alum-1  "#eeeeec" "alum-1"     :group 'salm :type '(color))
(defcustom
  mery-alum-2  "#d3d7cf" "alum-2"     :group 'salm :type '(color))
(defcustom
  mery-alum-3  "#babdb6" "alum-3"     :group 'salm :type '(color))
(defcustom
  mery-alum-4  "#888a85" "alum-4"     :group 'salm :type '(color))
(defcustom
  mery-alum-5  "#555753" "alum-5"     :group 'salm :type '(color))
(defcustom
  mery-alum-6  "#2e3436" "alum-6"     :group 'salm :type '(color))

(defcustom
  mery-cham-0  "#b4fa70" "cham-0"     :group 'salm :type '(color))
(defcustom
  mery-blue-0  "#8cc4ff" "blue-0"     :group 'salm :type '(color))
(defcustom
  mery-plum-0  "#e6a8df" "plum-0"     :group 'salm :type '(color))
(defcustom
  mery-red-0  "#ff4b4b" "red-0"     :group 'salm :type '(color))
(defcustom
  mery-alum-5.5  "#41423f" "alum-5.5"     :group 'salm :type '(color))
(defcustom
  mery-alum-7  "#212526" "alum-7"     :group 'salm :type '(color))
(defcustom
  mery-diff-green  "#00af00" "diff-green"     :group 'salm :type '(color))
(defcustom
  mery-diff-red  "#d70000" "diff-red"     :group 'salm :type '(color))

;; Background (dark).
(defcustom
  mery-base03    "#002B36" "Dark base 3"     :group 'salm :type '(color))
(defcustom
  mery-base02    "#073642" "Dark base 2"     :group 'salm :type '(color))

;; Content
(defcustom
  mery-base01    "#586E75" "Dark content 1"  :group 'salm :type '(color))
(defcustom
  mery-base00    "#657B83" "Dark content 0"  :group 'salm :type '(color))
(defcustom
  mery-base0     "#839496" "Light content 0" :group 'salm :type '(color))
(defcustom
  mery-base1     "#93A1A1" "Light content 1" :group 'salm :type '(color))

;; Background (light)
(defcustom
  mery-base2     "#EEE8D5" "Light base 2"    :group 'salm :type '(color))
(defcustom
  mery-base3     "#FDF6E3" "Light base 3"    :group 'salm :type '(color))

;; Basic colors
(defcustom
 mery-yellow    "#B58900" "Yellow"          :group 'salm :type '(color))
(defcustom
 mery-orange    "#CB4B16" "Orange"          :group 'salm :type '(color))
(defcustom
 mery-red       "#DC322F" "Red"             :group 'salm :type '(color))
(defcustom
 mery-magenta   "#D33682" "Magenta"         :group 'salm :type '(color))
(defcustom
 mery-violet    "#6C71C4" "Violet"          :group 'salm :type '(color))
(defcustom
 mery-blue      "#268BD2" "Blue"            :group 'salm :type '(color))
(defcustom
 mery-cyan      "#2AA198" "Cyan"            :group 'salm :type '(color))
(defcustom
 mery-green     "#859900" "Green"           :group 'salm :type '(color))

;; Darker colors
(defcustom
 mery-yellow-d  "#7B6000" "Dark yellow"     :group 'salm :type '(color))
(defcustom
 mery-orange-d  "#8B2C02" "Dark orange"     :group 'salm :type '(color))
(defcustom
 mery-red-d     "#990A1B" "Dark red"        :group 'salm :type '(color))
(defcustom
 mery-magenta-d "#93115C" "Dark magenta"    :group 'salm :type '(color))
(defcustom
 mery-violet-d  "#3F4D91" "Dark violet"     :group 'salm :type '(color))
(defcustom
 mery-blue-d    "#00629D" "Dark blue"       :group 'salm :type '(color))
(defcustom
 mery-cyan-d    "#00736F" "Dark cyan"       :group 'salm :type '(color))
(defcustom
 mery-green-d   "#546E00" "Dark green"      :group 'salm :type '(color))

;; Lighter colors
(defcustom
 mery-yellow-l  "#DEB542" "Light yellow"    :group 'salm :type '(color))
(defcustom
 mery-orange-l  "#F2804F" "Light orange"    :group 'salm :type '(color))
(defcustom
 mery-red-l     "#FF6E64" "Light red"       :group 'salm :type '(color))
(defcustom
 mery-magenta-l "#F771AC" "Light magenta"   :group 'salm :type '(color))
(defcustom
 mery-violet-l  "#9EA0E5" "Light violet"    :group 'salm :type '(color))
(defcustom
 mery-blue-l    "#69B7F0" "Light blue"      :group 'salm :type '(color))
(defcustom
 mery-cyan-l    "#69CABF" "Light cyan"      :group 'salm :type '(color))
(defcustom
 mery-green-l   "#B4C342" "Light green"     :group 'salm :type '(color))

(custom-theme-set-variables
 'salm
 )

(custom-theme-set-faces
 'salm
 `(bold ((t (:weight bold))))
 `(compilation-column-number ((t (:foreground ,mery-butter-2))))
 `(cursor ((t (:background "grey80"))))
 `(custom-button ((t (:background "#2D3232" :foreground "#DDD" :box (:line-width 2 :style released-button)))))
 `(custom-state ((t (:foreground ,green))))
 `(default ((t (:inherit nil :stipple nil :background ,mery-alum-6  :foreground ,mery-alum-1
			 :inverse-video nil :box nil :strike-through nil :overline nil
			 :underline nil :slant normal :weight normal
			 :height 90 :width normal :foundry "unknown"
			 :family "Knack Nerd Font"))))
 `(diff-added ((t (:foreground ,green))))
 `(diff-file-header ((((class color) (min-colors 88) (background dark)) (:weight bold))))
 `(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground ,violet))))
 `(diff-refine-added ((t (:inherit diff-refine-change :foreground "green"))))
 `(diff-refine-change ((t (:background "grey15"))) t)
 `(diff-refine-changed ((t (:background "grey15"))))
 `(diff-refine-removed ((t (:inherit diff-refine-change :foreground "red"))))
 `(diff-removed ((t (:foreground ,red-d))))
 `(dired-directory ((t (:foreground ,blue))))
 `(diredp-compressed-file-suffix ((t (:foreground "grey50"))))
 `(diredp-date-time ((t (:foreground ,violet))))
 `(diredp-dir-heading ((t (:foreground ,green))))
 `(diredp-dir-name ((t (:foreground ,blue))))
 `(diredp-dir-priv ((t (:foreground ,blue-l))))
 `(diredp-exec-priv ((t (:foreground ,green))))
 `(diredp-file-name ((t (:inherit default))))
 `(diredp-file-suffix ((t (:foreground "grey70"))))
 `(diredp-ignored-file-name ((t (:foreground ,base01))))
 `(diredp-no-priv ((t nil)))
 `(diredp-number ((t (:foreground "grey50"))))
 `(diredp-rare-priv ((t (:foreground ,cyan-l))))
 `(diredp-read-priv ((t (:foreground "grey70"))))
 `(diredp-symlink ((t (:foreground ,cyan))))
 `(diredp-write-priv ((t (:foreground "grey90"))))
 `(erc-action-face ((t (:slant italic))))
 `(erc-current-nick-face ((t (:foreground ,cyan :weight bold))))
 `(erc-input-face ((t (:foreground ,orange))))
 `(erc-my-nick-face ((t (:foreground ,orange :weight bold))))
 `(erc-nick-default-face ((t (:foreground "#A3B1B1"))))
 `(erc-notice-face ((t (:foreground ,violet))))
 `(erc-prompt-face ((t (:foreground "white" :weight bold))))
 `(erc-timestamp-face ((t (:foreground ,green))))
 `(error ((t (:foreground ,red :weight bold))))
 `(escape-glyph ((((background dark)) (:foreground "cyan"))))
 `(eshell-prompt ((t (:foreground ,green :weight bold))))
 `(font-latex-italic-face ((t (:inherit italic :foreground ,green))))
 `(font-latex-math-face ((t (:foreground ,red))))
 `(font-latex-sectioning-5-face ((t (:inherit nil :foreground ,yellow :weight bold))))
 `(font-latex-sedate-face ((t (:foreground ,cyan-d))))
 `(font-latex-string-face ((t (:foreground ,orange))))
 `(font-latex-warning-face ((t (:inherit bold :foreground ,red))))
 `(font-lock-builtin-face ((t (:foreground ,mery-plum-1))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-comment-face ((t (:foreground ,violet :slant normal))))
 `(font-lock-constant-face ((t (:foreground ,base1))))
 `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground ,violet))))
 `(font-lock-function-name-face ((t (:inherit default :foreground "#C3D1D1"))))
 `(font-lock-keyword-face ((t (:foreground ,base00 :weight bold))))
 `(font-lock-preprocessor-face ((nil (:foreground ,magenta))))
 `(font-lock-string-face ((t (:foreground "#d7af87"))))
 `(font-lock-type-face ((t (:foreground ,green :weight normal))))
 `(font-lock-variable-name-face ((t (:foreground "#E3F1F1" :weight normal))))
 `(font-lock-warning-face ((t (:foreground ,red))))
 `(fringe ((((class color) (background dark)) (:background ,mery-alum-7))))
 `(ggtags-global-line ((t (:inherit secondary-selection))))
 `(header-line ((t (:inherit mode-line))))
 `(highlight-beyond-fill-column-face ((t (:background ,red-d))))
 `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,green))))
 `(ivy-current-match ((t (:background ,green-d))))
 `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,red))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit minibuffer-prompt))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit minibuffer-prompt))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit minibuffer-prompt))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit minibuffer-prompt))))
 `(swiper-line-face ((t (:background "#354900"))))
 `(swiper-match-face-1 ((t (:background ,cyan-d))))
 `(swiper-match-face-2 ((t (:background ,green-d))))
 `(swiper-match-face-3 ((t (:background ,cyan-d))))
 `(swiper-match-face-4 ((t (:background ,green-d))))
 `(ivy-remote ((t (:foreground ,violet))))
 `(jabber-chat-prompt-foreign ((t (:foreground ,cyan-d :weight bold))))
 `(jabber-chat-prompt-local ((t (:foreground ,green :weight bold))))
 `(jabber-rare-time-face ((t (:foreground ,violet :weight bold))))
 `(jabber-roster-user-away ((t (:foreground ,green-d :slant italic :weight normal))))
 `(jabber-roster-user-online ((t (:foreground ,green-l :slant normal :weight bold))))
 `(jabber-title-large ((t (:foreground "white" :weight bold))))
 `(jabber-title-medium ((t (:weight bold :height 1.0 :width expanded))))
 `(link ((((class color) (min-colors 88) (background dark)) (:foreground ,blue :inverse-video nil :underline t))))
 `(link-visited ((default (:inherit link)) (((class color) (background dark)) (:foreground ,violet))))
 `(magit-bisect-bad ((t (:foreground ,red))))
 `(magit-bisect-good ((t (:foreground ,green))))
 `(magit-bisect-skip ((t (:foreground ,yellow))))
 `(magit-blame-heading ((t (:background "grey15" :foreground "grey80"))))
 `(magit-branch-current ((t (:inherit magit-branch-local :box 1))))
 `(magit-branch-local ((t (:foreground ,cyan))))
 `(magit-branch-remote ((t (:foreground ,green))))
 `(magit-cherry-equivalent ((t (:foreground ,magenta-d))))
 `(magit-cherry-unmatched ((t (:foreground ,cyan-d))))
 `(magit-diff-added ((t (:foreground ,green))))
 `(magit-diff-added-highlight ((t (:foreground ,green))))
 `(magit-diff-base ((t (:background ,green-d :foreground "gray80"))))
 `(magit-diff-base-highlight ((t (:background ,green-d :foreground "white"))))
 `(magit-diff-context ((t (:foreground "grey70"))))
 `(magit-diff-context-highlight ((t (:foreground "grey80"))))
 `(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight :foreground ,red))))
 `(magit-diff-hunk-heading ((t (:foreground ,violet))))
 `(magit-diff-hunk-heading-highlight ((t (:foreground ,violet :weight bold))))
 `(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :foreground ,red))))
 `(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading-highlight :background ,red :foreground "grey80"))))
 `(magit-diff-removed ((t (:foreground ,red))))
 `(magit-diff-removed-highlight ((t (:foreground "red"))))
 `(magit-diffstat-added ((t (:inherit diff-added))))
 `(magit-diffstat-removed ((t (:inherit diff-removed))))
 `(magit-item-highlight ((t (:background "grey15"))))
 `(magit-log-author ((t (:foreground ,red))))
 `(magit-process-ng ((t (:inherit magit-section-heading :foreground ,red))))
 `(magit-process-ok ((t (:inherit magit-section-heading :foreground ,green))))
 `(magit-reflog-amend ((t (:foreground ,magenta))))
 `(magit-reflog-checkout ((t (:foreground ,blue))))
 `(magit-reflog-cherry-pick ((t (:foreground ,green))))
 `(magit-reflog-commit ((t (:foreground ,green))))
 `(magit-reflog-merge ((t (:foreground ,green))))
 `(magit-reflog-other ((t (:foreground ,cyan))))
 `(magit-reflog-rebase ((t (:foreground ,magenta))))
 `(magit-reflog-remote ((t (:foreground ,cyan))))
 `(magit-reflog-reset ((t (:foreground ,red))))
 `(magit-section-heading ((t (:foreground ,green :weight bold))))
 `(magit-section-heading-selection ((t (:foreground ,red))))
 `(magit-sequence-drop ((t (:foreground ,red))))
 `(magit-sequence-head ((t (:foreground ,blue))))
 `(magit-sequence-part ((t (:foreground ,yellow))))
 `(magit-sequence-stop ((t (:foreground ,green))))
 `(magit-signature-bad ((t (:foreground ,red :weight bold))))
 `(magit-signature-error ((t (:foreground ,red))))
 `(magit-signature-expired ((t (:foreground ,orange))))
 `(magit-signature-good ((t (:foreground ,green))))
 `(magit-signature-revoked ((t (:foreground ,violet))))
 `(magit-signature-untrusted ((t (:foreground ,cyan))))
 `(magit-tag ((t (:foreground ,yellow))))
 `(magit-section-highlight ((t (:background "grey15"))))
 `(minibuffer-prompt ((((background dark)) (:foreground "white"))))
 `(mode-line ((t (:background "#2D3232" :foreground "#AFB3B2" :box nil))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id :foreground "gray30"))))
 `(mode-line-inactive ((t (:inherit mode-line :background "#222727" :foreground "gray30"))))
 `(nobreak-space ((((class color) (min-colors 88)) (:inherit escape-glyph :underline t))))
 `(powerline-active0 ((t (:inherit mode-line :background "#121717"))))
 `(powerline-active1 ((t (:inherit mode-line :background "#222727"))))
 `(powerline-active2 ((t (:inherit mode-line :background "#222727"))))
 `(powerline-inactive0 ((t (:inherit mode-line-inactive :background "black"))))
 `(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#121717"))))
 `(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#121717"))))
 `(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#93A1A1"))))
 `(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#8BA898"))))
 `(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#8391A1"))))
 `(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#7B9888"))))
 `(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#738191"))))
 `(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#6B8878"))))
 `(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#637181"))))
 `(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#5B7868"))))
 `(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#536171"))))
 `(region ((t (:background "#354900"))))
 `(secondary-selection ((t (:background "#344E00"))))
 `(smerge-refined-change ((t (:background "grey20"))) t)
 `(smerge-refined-changed ((t (:background "grey20"))))
 `(spaceline-highlight-face ((t (:background "#272A2A" :foreground "#FFFFFF" :inherit (quote mode-line)))))
 `(spaceline-modified ((t (:background "#A66" :foreground "#3E3D31" :inherit (quote mode-line)))))
 `(spaceline-unmodified ((t (:background "#777" :foreground "#3E3D31" :inherit (quote mode-line)))))
 `(success ((t (:foreground ,green :weight bold))))
 `(warning ((t (:foreground ,yellow :weight bold))))
 `(whitespace-indentation ((t (:foreground "grey20"))))
 `(whitespace-newline ((t (:foreground "grey20" :weight normal))))
 `(whitespace-space ((t (:foreground "grey20"))))
 `(whitespace-space-after-tab ((t (:foreground "grey20"))))
 `(whitespace-tab ((t (:foreground "grey20"))))
 `(widget-field ((t (:background "#222" :box (:line-width 2 :color "#555")))))
 `(woman-addition ((t (:inherit font-lock-builtin-face :foreground ,blue))))
 `(woman-bold ((t (:inherit bold :foreground ,green))))
 `(woman-italic ((t (:inherit italic :slant italic)))))

(provide-theme 'salm)
(provide 'salm)
;;; salm-theme.el ends here
