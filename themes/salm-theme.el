;;;; package --- Summary

;;; Commentary:

;;; Code:
(deftheme salm
  "some fun with colors and dirty hands Rémy.")

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
 `(region ((t (:background ,mery-plum-3))))
 `(default ((t (:inherit nil :stipple nil :background ,mery-alum-6  :foreground ,mery-alum-1
			 :inverse-video nil :box nil :strike-through nil :overline nil
			 :underline nil :slant normal :weight normal
			 :height 96 :width normal :foundry "unknown"
			 :family "Knack Nerd Font"))))


 `(error ((t (:foreground ,mery-plum-3 :weight bold))))

 `(font-latex-italic-face ((t (:inherit italic :foreground ,mery-cham-3))))
 `(font-latex-math-face ((t (:foreground ,mery-red-3))))
 `(font-latex-sectioning-5-face ((t (:inherit nil :foreground ,mery-yellow :weight bold))))
 `(font-latex-sedate-face ((t (:foreground ,mery-cyan-d))))
 `(font-latex-string-face ((t (:foreground ,mery-butter-1))))
 `(font-latex-warning-face ((t (:inherit bold :foreground ,mery-red-3))))
 `(font-lock-builtin-face ((t (:foreground ,mery-choc-2))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-comment-face ((t (:foreground ,mery-cham-3 :slant normal))))
 `(font-lock-constant-face ((t (:foreground ,mery-butter-3))))
 `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground ,mery-violet))))
 `(font-lock-function-name-face ((t (:inherit default :foreground "#C3D1D1"))))
 `(font-lock-keyword-face ((t (:foreground ,mery-base00 :weight bold))))
 `(font-lock-preprocessor-face ((nil (:foreground ,mery-magenta))))
 `(font-lock-string-face ((t (:foreground "#d7af87"))))
 `(font-lock-type-face ((t (:foreground ,mery-cham-3 :weight normal))))
 `(font-lock-variable-name-face ((t (:foreground "#E3F1F1" :weight normal))))
 `(font-lock-warning-face ((t (:foreground ,mery-orange-1))))
 `(fringe ((((class color) (background dark)) (:background ,mery-alum-7))))
 `(ggtags-global-line ((t (:inherit secondary-selection))))
 `(header-line ((t (:inherit mode-line))))
 `(highlight-beyond-fill-column-face ((t (:background ,mery-red-2))))
 `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,mery-cham-3))))
 `(ivy-current-match ((t (:background ,mery-cham-2))))
 `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,mery-red-3))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit minibuffer-prompt))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit minibuffer-prompt))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit minibuffer-prompt))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit minibuffer-prompt))))

 `(ivy-remote ((t (:foreground ,mery-violet))))
;; `(markdown-link-face ((t (:foreground ,mery-base00))))
;; `(markdown-url-face ((t (:foreground  ,mery-choc-2))))
 `(link ((((class color) (min-colors 88) (background dark)) (:foreground ,mery-blue-1 :inverse-video nil :underline t))))
 `(link-visited ((default (:inherit link)) (((class color) (background dark)) (:foreground ,mery-plum-0))))
 `(magit-bisect-bad ((t (:foreground ,mery-red-3))))
 `(magit-bisect-good ((t (:foreground ,mery-cham-3))))
 `(magit-bisect-skip ((t (:foreground ,mery-yellow))))
 `(magit-blame-heading ((t (:background "grey15" :foreground "grey80"))))
 `(magit-branch-current ((t (:inherit magit-branch-local :box 1))))
 `(magit-branch-local ((t (:foreground ,mery-cyan))))
 `(magit-branch-remote ((t (:foreground ,mery-cham-3))))
 `(magit-cherry-equivalent ((t (:foreground ,mery-magenta-d))))
 `(magit-cherry-unmatched ((t (:foreground ,mery-cyan-d))))
 `(magit-diff-added ((t (:foreground ,mery-cham-3))))
 `(magit-diff-added-highlight ((t (:foreground ,mery-cham-3))))
 `(magit-diff-base ((t (:background ,mery-cham-2 :foreground "gray80"))))
 `(magit-diff-base-highlight ((t (:background ,mery-cham-2 :foreground "white"))))
 `(magit-diff-context ((t (:foreground "grey70"))))
 `(magit-diff-context-highlight ((t (:foreground "grey80"))))
 `(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight :foreground ,mery-red-3))))
 `(magit-diff-hunk-heading ((t (:foreground ,mery-plum-0))))
 `(magit-diff-hunk-heading-highlight ((t (:foreground ,mery-plum-0 :weight bold))))
 `(success ((t (:foreground ,mery-cham-3 :weight bold))))
 `(warning ((t (:foreground ,mery-yellow :weight bold)))))


(provide-theme 'salm)

;;; salm-theme.el ends here
