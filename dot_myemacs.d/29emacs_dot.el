;; init.el --- init file for config emacs

;; Copyright (C) 2023 Rafael Tieppo

;; Author: Rafael Cesar Tieppo>
;; URL: http://github.com/rafatieppo/
;; Version: 1.0
;; Keywords: packages installation
;; Package-Requires:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; init.el file for EMACS (29) by Rafael Tieppo.
;; Most of all from this content was from internet.
;; Feel free to copy and share.
;;=============================================================================
;;; Code:

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'package)

;;STANDARD SETTINGS
;;===========================================================================
;; Neotree A emacs tree plugin like NERD tree for Vim.
(use-package neotree
             :ensure t
             :bind (([f8] . neotree-toggle))
             :config (setq neo-autorefresh nil))

;; Powerlines https://github.com/milkypostman/powerline/
;(use-package powerline
;             :ensure t
;             :config (powerline-default-theme))

;; No welcome windows ;; http://blog.droidzone.in/2012/05/22/remove-startup-split-screen-in-emacs/
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]

;; Vertical or horizontal buffer
;(setq split-width-threshold 9999)
(setq split-width-threshold 0)

;; Font and  size
;(set-frame-font "Anonymous Pro-14.5")
;(set-frame-font "Envy Code R-17")
(set-frame-font "Fira Code-15")
;(set-frame-font "Hack-16")
;(set-frame-font "IBMPlexMono-17")
;(set-frame-font "JetBrains Mono-17")
;(set-frame-font "JetBrains Mono 16" nil t) ;; new code to set font
;(set-frame-font "Monaco-16.5")
;(set-frame-font "monofur-19")
;(set-frame-font "Menlo-16")
;(set-frame-font "Monoid-14")
;(set-frame-font "Source Code Pro-17")
;(set-frame-font "Tex Gyre Adventor-11")

;; Space between lines and between line numbers and text (like margin)
(setq-default line-spacing 3) 
(setq-default left-fringe-width  15)
;(setq-default right-fringe-width  0)
;(set-face-attribute 'fringe nil :background "black")

;; Provide Highlight code in SRC
(setq org-src-fontify-natively t)

;; line numbers
(global-display-line-numbers-mode 1)
;(global-linum-mode 1)

;; cursor position from left margin
(setq column-number-mode t)

;; Show file name in title bar
(setq frame-title-format "%b - Emacs")

;; Break lines standard is  72
(setq-default fill-column 72)

;; Does not truncate lines
;(setq-default truncate-lines t)

;; Auto break long lines.
;(setq-default auto-fill-function 'do-auto-fill)

;; Wrap long lines
(global-visual-line-mode t)
(setq-default word-wrap t)

;; Screen lines and not logical lines)
(visual-line-mode 1)

;; Turn off auto save auto backup.
(setq auto-save-default nil) ;; Para o #autosave#.
(setq make-backup-files nil) ;; Para o backup~.
 
;; Spaces instead tabs
(setq-default indent-tabs-mode nil)

;; cua mode
;(cua-mode t)
(cua-selection-mode t)

;; stop cursor blinking
(blink-cursor-mode 1)

;; removes menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Highlight matching pairs.
(show-paren-mode 1)

;;How to have emacs highlight text selections
;;	(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing

;; evil-mode
;(require 'evil)
;(evil-mode 1)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

;;ORG MODE
;;===========================================================================
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; files for org agendasorg-agenda
(setq org-agenda-files (list "/home/rafatieppo/Dropbox/emacs_org_mode/rafa_tieppo.org"
			     "/home/rafatieppo/Dropbox/emacs_org_mode/proj_extens.org"
			     "/home/rafatieppo/Dropbox/emacs_org_mode/proj_person.org"
			     "/home/rafatieppo/Dropbox/emacs_org_mode/proj_resear.org"
			     "/home/rafatieppo/Dropbox/emacs_org_mode/proj_teachi.org"
                        ))

;; bullets instead *
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("✿" "✸" "◉" "●" "○" "☯" "☢"))) ;; Small ► • ★ ▸

;; arrow instead ...
(setq org-ellipsis " ⤵") ;;" ▾"

;; babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)
    (R . t)))

;; org-tree presentation org-tree-slide-mode! Navigate slides with C-< and C->
(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

;; packs to print calendar with appointments as GoogleCalendar
; (require 'calfw)
; (require 'calfw-org)
; (setq cfw:org-agenda-schedule-args '(:timestamp))
; (require 'calfw-ical)

;; ORG mode CLASSES and COLORS for TASKS
(setq org-todo-keywords
      '((sequence "BACKLOG(w@/!)" "THISWEEK(w@/!)" "TODAY(t)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-todo-keyword-faces
      '(
        ("BACKLOG" . (:foreground "orange" :weight bold))
        ("TODAY" . (:foreground "red" :weight bold))
        ("THISWEEK" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELED" . (:foreground "gray" :weight bold))
        ))

;; ORG CAPTURE I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
  '(    ;; ... other templates
    ("l" "Link" entry (file+headline 
         "~/Dropbox/emacs_org_mode/capture.org" "Link") 
         "* LINK %^{Description} %^g
         %?
         Added: %U")
    ("j" "Journal Entry"
         entry (file+datetree "~/Dropbox/emacs_org_mode/capture.org")
         "* %?"
         :empty-lines 1)
    ("p" "Phone" entry (file+headline 
         "~/Dropbox/emacs_org_mode/capture.org" "Phone") 
         "* NUMBER %^{Description} %^g
         %?
         Added: %U")
    ("q" "Quote" entry (file+headline 
         "~/Dropbox/emacs_org_mode/capture.org" "Quote") 
         "* QUOTE %^{Description} %^g
         %?
         Added: %U")
    ("t" "Ted Talks" entry (file+headline 
         "~/Dropbox/emacs_org_mode/capture.org" "Ted") 
         "* TED %^{Description} %^g
         %?
         Added: %U")
        ;; ... other templates
    ))

;; SPECIAL PROGRAMMING TOOLS
;;===========================================================================
;; multiple-cursors.el https://github.com/magnars/multiple-cursors.el
;(require 'multiple-cursors)

;;===========================================================================
;; raibow delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode
   :ensure t)
  
;;===========================================================================
;; ace-jump-mode.el https://github.com/winterTTr/ace-jump-mode
(global-set-key (kbd "C-c SPC") 'avy-goto-char)

;;===========================================================================
;; helm
;(require 'helm-config)
(require 'helm)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ack-grep")
  (setq helm-grep-default-command
        "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command
        "ack-grep -H --no-group --no-color %e %p %f"))

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-locate-fuzzy-match               t
      helm-apropos-fuzzy-match              t
      helm-lisp-fuzzy-completion            t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t)

(helm-mode 1)
(helm-autoresize-mode t)

(global-set-key (kbd "C-c h")   'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(add-to-list 'helm-sources-using-default-as-input
             'helm-source-man-pages)

;; function highlights lisp
(require 'highlight-symbol)

;; indent GUIDE https://raw.githubusercontent.com/zk-phi/indent-guide/master/indent-guide.el
(require 'indent-guide)
(setq indent-guide-recursive t)

;; highlight-indentation-mode
;(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; smart Parenthesis https://github.com/Fuco1/smartparens
;(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)

;; folding by indentation ;; git clone https://github.com/zenozeng/yafolding.el.git
(require 'yafolding)
(define-key yafolding-mode-map (kbd "C-c {") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "C-c }") 'yafolding-hide-parent-element)
(define-key yafolding-mode-map (kbd "C-c ]") 'yafolding-toggle-element)

;; add highlight for certain keywords
(make-face 'special-words) 
(set-face-attribute 'special-words nil :foreground "White" :background "Firebrick") 
(dolist
    (mode '(fundamental-mode
            gnus-article-mode
            org-mode
            shell-mode
            sh-mode
            muse-mode
            ess-mode
            ;polymode-mode
            python-mode
            markdown-mode
            TeX-mode
            latex-mode)) 
  (font-lock-add-keywords
   mode 
   '(("\\<\\(COMMENT\\|DONE\\|TODO\\|STOP\\|IMPORTANT\\|NOTE\\|OBS\\|ATTENTION\\|REVIEW\\)" 
      0 'font-lock-warning-face t) 
     ("\\<\\(BUG\\|WARNING\\|DANGER\\|FIXME\\)" 
      0 'special-words t)))
  ) 

;(use-package flycheck
;  :defer t
;  :hook (lsp-mode . flycheck-mode))
 
;(use-package flycheck
;  :ensure t
;  :init (global-flycheck-mode t))


;;AUTO COMLETE AND YASNIPPET
;;=========================================================================== 
(require 'yasnippet)
(yas-global-mode 1)

(defun yasnippet-snippets--fixed-indent ()
  "Set `yas-indent-line' to `fixed'."
  (set (make-local-variable 'yas-indent-line) 'fixed))

(defun yasnippet-snippets--no-indent ()
  "Set `yas-indent-line' to nil."
  (set (make-local-variable 'yas-indent-line) nil))

; add ac-sources to default ac-sources
(defun ac-LaTeX-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources)))
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)

;; R ESS
;;===========================================================================
;; setting to work with ess and r
(require 'ess-site)
;(require 'ess-eldoc)
;(setq-default ess-dialect "R")
;(setq-default inferior-R-args "--no-restore-history --no-save ")

;; key SHIFT + ENTER
(eval-after-load "ess-mode"
 '(progn
   ;;(define-key ess-mode-map [(control return)] nil)
   (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)))

;; To activate ESS auto-complete for R.
;(setq ess-use-auto-complete 'script-only)

;; cancel centering comments in R ESS
;(setf (cdr (assoc 'ess-indent-with-fancy-comments ess-own-style-list)) nil)

;; if you want all help buffers to go into one frame do:
;(setq ess-help-own-frame 'one)

;; MARKDOWN MODE 
;;===========================================================================
;; org momde minor mode markdown http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
(require 'org-table)
(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          (lambda()
            (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))

;; markdown enable MATH ;http://jblevins.org/projects/markdown-mode/
(setq markdown-enable-math t)

;; markdown extensions. (IT MUST BE BEFORE LATEX EXTENSIONS.)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org-struct minor mode active in markdown mode.
;(add-hook 'markdown-mode-hook 'turn-on-orgstruct)
;(add-hook 'markdown-mode-hook 'turn-on-orgstruct++)

;; REFTEX CITATION
;;===========================================================================
;; reftex citation pandoc http://www.unknownerror.org/opensource/jgm/pandoc/q/stackoverflow/13607156/autocomplete-pandoc-style-citations-from-a-bibtex-file-in-emacs http://tex.stackexchange.com/a/31992/5701

;; to use biber instead bibtex 
(setq TeX-parse-self t)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("/home/rafatieppo/Dropbox/bibtex/references.bib"))

;; How to solve @
(eval-after-load 'reftex-vars
  '(progn 
     (setq reftex-cite-format '((?\C-m . "@%l")
                                (?\C-l . "\\cite{%l\}")
                                (?\C-o . "\\citeonline{%l\}")
                                (?\C-t . "\\citet{%l\}")
                                (?\C-p . "\\citep{%l\}")
                                ))))

(require 'citeproc)

;; LATEX
;;===========================================================================
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(require 'company-auctex)
;; Naveg http://piotrkazmierczak.com/2010/05/13/emacs-as-the-ultimate-latex-editor/ C-c = 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; HTML
;;===========================================================================
;; html-mode
;(sp-with-modes '(html-mode sgml-mode web-mode)
;  (sp-local-pair "<" ">"))

;; https://www.emacswiki.org/emacs/IndentingHtml
    (add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))

;; web-mode
(setq css-indent-offset tab-width)
(use-package web-mode
  :init
  (setq web-mode-attr-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-css-indent-offset tab-width)
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-sql-indent-offset tab-width))

;; BASH
;;===========================================================================
(setq sh-basic-offset 2
      sh-indentation 2)
;; snippets, please
(add-hook 'sh-mode-hook 'yas-minor-mode)
;; on the fly syntax checking
(add-hook 'sh-mode-hook 'flycheck-mode)

;; C CONFIGURATION
;;===========================================================================
;; install apt-get install libclang-dev apt-get install cmake
;(add-hook 'after-init-hook 'global-company-mode)
; (add-hook 'c++-mode-hook 'irony-mode)
; (add-hook 'c-mode-hook 'irony-mode)
; (add-hook 'objc-mode-hook 'irony-mode)
; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;To have autocompletion when you are including headers:
;(add-to-list 'company-backends 'company-irony-c-headers)

;; PYTHON CONFIGURATION
;;===========================================================================
;;https://gitlab.com/skybert/my-little-friends/-/blob/master/emacs/.emacs
(use-package eglot
  :ensure t
  :hook
  ((python-mode . eglot-ensure))
  )

;; https://emacs-lsp.github.io/lsp-pyright/ ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/ ;; https://www.mattduck.com/lsp-python-getting-started.html
;; FOR lsp-pyright
;(use-package lsp-pyright
;  :ensure t
;  :hook (python-mode . (lambda ()
;                          (require 'lsp-pyright)
;                          (lsp))))  ;lsp or lsp-deferred 

;; PHP CONFIGURATION
;;===========================================================================

(use-package lsp-mode
 :config
 (setq lsp-prefer-flymake nil)
 :hook (php-mode . lsp)
 :commands lsp)

;; install https://phpactor.readthedocs.io/en/master/usage/standalone.html
;; config https://phpactor.readthedocs.io/en/master/lsp/emacs.html
;; Add lsp or lsp-deferred function call to functions for your php-mode customization

;; Add lsp or lsp-deferred function call to functions for your php-mode customization
(defun init-php-mode ()
  (eglot-ensure))

(with-eval-after-load 'php-mode
  ;; If phpactor command is not installed as global, remove next ;; and write the full path
  (custom-set-variables '(lsp-phpactor-path "/home/rafatieppo/.local/share/phpactor.phar"))  
  (add-hook 'php-mode-hook #'init-php-mode))


;(defun init-php-mode ()
;  (lsp-deferred))

;(with-eval-after-load 'php-mode
;  ;; If phpactor command is not installed as global, write the full path
;  (custom-set-variables '(lsp-phpactor-path "/home/rafatieppo/.local/share/phpactor.phar"))
;  (add-hook 'php-mode-hook #'init-php-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; lsp-mode CONFIGURATION
;;===========================================================================
;; FOR lsp-mode       https://emacs-lsp.github.io/lsp-mode/page/installation/
;(use-package lsp-mode
;  :init
;  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;  (setq lsp-keymap-prefix "C-c l")
;  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;         (python-mode . lsp)
;         ;; if you want which-key integration
;         ;(lsp-mode . lsp-enable-which-key-integration)
;         )
;  :commands lsp)

;; to stop messages in minibuffer
;(setq lsp-headerline-breadcrumb-enable nil)

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;(use-package lsp-ui
;  :commands lsp-ui-mode)

;; optionally if you want to use debugger
; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;(use-package which-key
;    :config
;    (which-key-mode))
    
;;(use-package lsp-ui
;;  :config (setq lsp-ui-sideline-show-hover nil ;; t nil
;                lsp-ui-sideline-delay 0.5
;                lsp-ui-doc-delay 5
;;                lsp-ui-sideline-ignore-duplicates t
;                lsp-ui-doc-position 'bottom
;                lsp-ui-doc-alignment 'frame
;;                lsp-ui-doc-header nil
;;                lsp-ui-doc-include-signature nil
;;                lsp-ui-doc-use-childframe t)) 

;; pyvenv
;(use-package pyvenv
;  :config
;  (pyvenv-mode 1))

;; projectile is necessary to handle .git in folder, otherwise lsp wil not start
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'helm)) ; or ivy
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;;===========================================================================
;; MAGIT
;;===========================================================================
(use-package magit
  :ensure t)

;;===========================================================================
;;FUNCOES functions
;;===========================================================================
;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/lisp")
(load "/home/rafatieppo/.emacs.d/lisp/functions")
;(require 'functions)

;; THEMES - SEVERAL SCHEMES
;;===========================================================================
;; Solarized
;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/themess/solarized/")
;(require 'solarized-dark-theme)
;(require 'solarized-definitions)
;(require 'solarized-theme)
;(require 'color-theme-solarized)
;(require 'solarized-dark-theme)

(add-to-list 'load-path "/home/rafatieppo/.emacs.d/themess")
;(add-to-list 'custom-theme-load-path "/home/rafatieppo/.emacs.d/themess")
;(require 'afternoon-theme)
;(require 'ayu-theme)
;(require 'monokai-theme) ;; load first to improve ORG visualization
;;(require 'Amelie-theme)
;;(require 'ample-zen-theme)
;;(require 'assemblage-theme)
;(require 'atom-one-dark-theme)
;;(require 'blackboard-theme)
;;(require 'deep-thought-theme)
;;(require 'challenger-deep-theme.el)
;(require 'clues-theme)
;(require 'dracula-theme)
;;(require 'erosiond-theme)
;(require 'forest-blue-theme)
;;(require 'fogus-theme)
;;(require 'gotham-theme)
;;(require 'granger-theme)
;(require 'ibm-dark-theme)
;(require 'hydrangea-theme)
;;(require 'hickey-theme)
;;(require 'junio-theme)
;(require 'material-light-theme)
;(require 'material-theme)
;;(require 'moe-dark-theme)
;(require 'moe-light-theme)
;(require 'molokai-theme)
;(require 'nimbus-theme)
;(require 'oceanic-theme)
;;(require 'odersky-theme)
;(require 'seti-theme)
;;(require 'soothe-theme)
;(require 'spacemacs-dark-theme)
;;(require 'spolsky-theme)
;;(require 'tangotango-theme)
;(require 'tron-theme)
;;(require 'ujelly-theme)
;;(require 'underwater-theme)
;;(require 'wilmersdorf-theme)
;;(require 'wilson-theme)
;;(require 'zenburn-theme)
(require 'zerodark-theme)
;(require 'zonokai-blue-theme)
;(require 'color-theme-tomorrow)
;(color-theme-tomorrow--define-theme night-blue)
;(color-theme-tomorrow--define-theme night-eighties)
;;(color-theme-tomorrow--define-theme night-bright)
;;(color-theme-tomorrow--define-theme night)
;;(color-theme-tomorrow--define-theme day)

;(if (display-graphic-p) 
;  (require 'granger-theme) 
;  (require 'tron-theme))

;;===========================================================================
;;Horizontal line
;;===========================================================================
;(global-hl-line-mode 1)
;; Underline in current line
;(set-face-attribute hl-line-face nil :underline t)
;(set-face-background hl-line-face "#2F2F2F")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zerodark))
 '(custom-safe-themes
   '("2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "43ee7172f7ad20c70da9c42061e7f1e4e69eb9605fd0ed58900c7ad5c5fdfa94" "846ef3695c42d50347884515f98cc359a7a61b82a8d0c168df0f688cf54bf089" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(php-mode citeproc company-auctex eglot lsp-mode avy company lsp-pyright yasnippet yafolding web-mode use-package rainbow-delimiters py-autopep8 projectile org-tree-slide org-bullets neotree magit lsp-ui indent-guide highlight-symbol helm evil ess auto-complete auctex all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
