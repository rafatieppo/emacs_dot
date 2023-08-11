;;; oceanic-theme.el --- Oceanic theme.

;; Copyright (C) 2016, Tengfei Guo <terryr3rd@yeah.net>

;; Author: Tengfei Guo
;; Keywords: oceanic color theme
;; URL: https://github.com/terry3/oceanic-theme
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(deftheme oceanic
  "A color theme based on the Oceanic.")

;; base 00: #1B2B34
;; base 01: #343D46
;; base 02: #4F5B66
;; base 03: #65737E
;; base 04: #A7ADBA
;; base 05: #C0C5CE
;; base 06: #CDD3DE
;; base 07: #D8DEE9
;; base 08: #EC5f67
;; base 09: #F99157
;; base 0A: #FAC863
;; base 0B: #99C794
;; base 0C: #5FB3B3
;; base 0D: #6699CC
;; base 0E: #C594C5
;; base 0F: #AB7967


(custom-theme-set-faces
  'oceanic
  '(default ((t (:foreground "#c2c7d1" :background "#282B35")))) ;;D8DEE9 back 1B2B34
  '(cursor ((t (:background "#457a7e")))) ;;6699CC
  '(fringe ((t (:background "#1a1a1a"))))
  '(region ((t (:background "#343D46"))))
  '(font-lock-builtin-face ((t (:foreground "#bb9385" :weight normal)))) ;;FAC863
  '(font-lock-comment-face ((t (:foreground "#4F5B66" :weight normal))))
  '(font-lock-function-name-face ((t (:foreground "#D8DEE9" :weight normal)))) ;;D8DEE9
  '(font-lock-keyword-face ((t (:foreground "#5FB3B3" :weight normal))))
  '(font-lock-string-face ((t (:foreground "#9CBECC" :weight normal)))) ;;99C794
  '(font-lock-type-face ((t (:foreground "#C594C5" :weight normal))))
  '(font-lock-constant-face ((t (:foreground "#EC5f67" :weight normal))))
  '(font-lock-variable-name-face ((t (:foreground "#5b94ab" :weight normal)))) ;;C594C5
  '(minibuffer-prompt ((t (:foreground "#6699CC" :weight normal))))
  '(font-lock-warning-face ((t (:foreground "#EC5f67"  :weight normal))))
  '(highlight ((t (:background "#343D46" :weight normal))))
  '(linum ((t (:foreground "#195466" :weight normal)))) ;; #AB7967
  '(mode-line ((t (:background "#091f2e" :foreground "#cbd9db"
                               :box "black" :weight normal)))) ;;back 6699CC 506569 foreg 1B2B34 273031   box 6699CC
  '(mode-line-highlight ((t (:box nil))))

  '(show-paren-match ((t (:background "#FAC863"))))
  '(show-paren-mismatch ((t (:background "#EC5f67"))))

   ;; org-mode
  `(org-level-1                              ((t (:inherit default :foreground "#cb4b16" :weight bold :height 1.15))))
  `(org-level-2                              ((t (:inherit default :foreground "#859900" :weight bold :height 1.10))))
  `(org-level-3                              ((t (:inherit default :foreground "#268bd2" :weight bold :height 1.05))))
  `(org-level-4                              ((t (:inherit default :foreground "#5b94ab" :weight bold :height 1.00))))
  `(org-level-5                              ((t (:inherit default :foreground "#6c71c4" :weight bold :height 1.00))))
  `(org-level-6                              ((t (:inherit default :foreground "#6c71c4" :weight bold :height 1.00))))

  ;; Markdown
  '(markdown-code-face     ((t (:inherit font-lock-keyword-face))))
  '(markdown-pre-face      ((t (:inherit font-lock-keyword-face))))
  '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "#cb4b16" :weight bold :height 1.15)))) ;; base #5b94ab
  '(markdown-header-face-2 ((t (:inherit markdown-header-face :foreground "#859900" :weight bold :height 1.10))))
  '(markdown-header-face-3 ((t (:inherit markdown-header-face :foreground "#268bd2" :weight bold :height 1.05))))
  '(markdown-header-face-4 ((t (:inherit markdown-header-face :foreground "#5b94ab" :weight bold :height 1.00))))
  '(markdown-header-face-5 ((t (:inherit markdown-header-face :foreground "#6c71c4" :weight bold :height 1.00))))
  '(markdown-header-face-6 ((t (:inherit markdown-header-face :foreground "#6c71c4" :weight bold :height 1.00))))

  `(markdown-inline-code-face ((t (:inherit markdown-header-face :foreground "#5b94ab"))))
  `(markdown-italic-face ((t (:inherit italic))))
  `(markdown-link-face ((t (:inherit markdown-header-face :foreground "cyan" :weight normal))))
  `(markdown-markup-face ((t (:inherit markdown-header-face :foreground "#d3d3d3"))))
  `(markdown-reference-face ((t (:inherit markdown-link-face))))
  `(markdown-url-face ((t (:inherit markdown-header-face :foreground "#D8DEE9" :underline t :weight normal))))

  )

;; Autoload for MELPA

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'oceanic)

;;; oceanic-theme.el ends here
