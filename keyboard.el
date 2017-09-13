;;; KEYBOARD.EL --- map function keys to elisp funs.

;; Copyright (C) 1996 Remy TOMASETTO

;; Author: Remy TOMASETTO <Remy.TOMASETTO@sxb.bsf.alcatel.fr>
;; Maintainer: Remy TOMASETTO <Remy.TOMASETTO@sxb.bsf.alcatel.fr>
;; Created: 15 Nov 1996
;; Last modification: %G%
;; Version: %I%
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

(defconst keyboard-version "%W%"
  "keyboard-version-id

Report bugs to: Remy TOMASETTO <Remy.TOMASETTO@sxb.bsf.alcatel.fr>")




(provide 'keyboard)



(global-set-key [f1]  'goto-line)
(global-set-key [S-f1]  'what-line)
(global-set-key [M-f1]  'mark-c-function)
(global-set-key [f2]  'delete-other-windows)
(global-set-key [f3]  'compile)
(global-set-key [f4]  'next-error)
(global-set-key [f5]  'previous-error)
(global-set-key [f6]  'vc-toggle-read-only)


(global-set-key [C-right] 'jde-javadoc-autodoc-at-line) 
(global-set-key [C-left] 'jde-complete-at-point) 
(global-set-key [C-up] 'senator-previous-token)
(global-set-key [C-down] 'senator-next-token)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)

;;; KEYBOARD.EL ends here
