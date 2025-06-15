;; init.el --- init file for config emacs

;; Copyright (C) 2025 Rafael Tieppo

;; Author: Rafael Cesar Tieppo>
;; URL: http://github.com/rafatieppo/
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

;; First, ensure use-package is installed (from ELPA or MELPA)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This line is crucial for use-package to work correctly
(eval-when-compile (require 'use-package))
;; Set up package archives again, just in case use-package is installed first
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;;----------------------------------------------------------------------
;;STANDARD SETTINGS
;;----------------------------------------------------------------------

;; No welcome windows ;; http://blog.droidzone.in/2012/05/22/remove-startup-split-screen-in-emacs/
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]

;; Vertical or horizontal buffer
;(setq split-width-threshold 9999)
(setq split-width-threshold 0)

;; Font and  size
;(set-frame-font "Anonymous Pro-14.5")
;(set-frame-font "Envy Code R-17")
;(set-frame-font "Fira Code-15")
;(set-frame-font "Fira Code-15" nil t)
;(set-frame-font "JetBrains Mono 16" nil t) ;; new code to set font
;(set-frame-font "Hack-16")
(set-frame-font "IBMPlexMono-15" nil t)
;(set-frame-font "JetBrains Mono-15" nil t)
;(set-frame-font "Monaco-16.5")
;(set-frame-font "monofur-19")
;(set-frame-font "Menlo-16")
;(set-frame-font "Menlo-15" nil t)
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
;;(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete selected text when typing

;;----------------------------------------------------------------------
;; Evil 
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

;; ivy
;;----------------------------------------------------------------------
(use-package ivy 
  :ensure t
  :config
  (ivy-mode 1)
  )
(setopt ivy-use-virtual-buffers t)
(setopt ivy-count-format "(%d/%d) ")

;; avy
;;----------------------------------------------------------------------
(use-package avy
  :ensure t ; Garante que o pacote seja instalado se ainda não estiver

  ;; Configurações básicas
  :config
  (setq avy-background t) ; Torna o fundo do buffer mais escuro para destacar as letras de salto
  (setq avy-all-windows nil) ; Não mostra labels em outras janelas por padrão
  ;; (setq avy-timeout-seconds 0.5) ; Tempo limite para a seleção de caracteres (opcional)

  ;; Ativações de comandos e atalhos de teclado
  :bind
  ;; Para saltar para um caractere
  ("C-:" . avy-goto-char)
  ;; Para saltar para uma palavra ou linha (com base nas duas letras)
  ("C-'" . avy-goto-char-2)
  ;; Para saltar para uma linha
  ("M-g f" . avy-goto-line)
  )

;;----------------------------------------------------------------------
;; Icons and Neotree
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

;; Neotree A emacs tree plugin like NERD tree for Vim.
(use-package neotree
             :ensure t
             :bind (([f8] . neotree-toggle))
             :config 
             (setq neo-autorefresh nil)
             (setq neo-theme 'icons)
             )

;;----------------------------------------------------------------------
;; ORG-MODE
;;----------------------------------------------------------------------
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; files for org agendasorg-agenda
(setq org-agenda-files (list "/home/rafatieppo/Gdrive/emacs_org_mode/rafa_tieppo.org"
			     "/home/rafatieppo/Gdrive/emacs_org_mode/proj_extens.org"
			     "/home/rafatieppo/Gdrive/emacs_org_mode/proj_manage.org"			     
			     "/home/rafatieppo/Gdrive/emacs_org_mode/proj_person.org"
			     "/home/rafatieppo/Gdrive/emacs_org_mode/proj_resear.org"
			     "/home/rafatieppo/Gdrive/emacs_org_mode/proj_teachi.org"
                        ))
                                                                
(setq org-agenda-custom-commands
      '(("Z" "tags-todo -DONE"
         (
          (tags-todo "personal/-DONE")
          (tags-todo "u_cetegeo/-DONE")                    
          (tags-todo "u_extension/-DONE")          
          (tags-todo "u_ppgasp/-DONE")
          (tags-todo "u_research/-DONE")
          (tags-todo "u_teaching/-DONE")
          (tags-todo "usedados/-DONE")
          ;(tags "-{.*}")
          ))))

;; to use \ref{} commands, because org-mode generates auto labels for figs and table 
(setq org-latex-prefer-user-labels t)

;; for org mode to export block source minted
; (setq org-latex-pdf-process (list "pdflatex -shell-escape %f")) it was generating erro on \ref{tbl:tb01} or [[tbl:tb01]] 
; http://joonro.github.io/blog/posts/org-mode-outputdir-minted-latex-export/
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(add-to-list 'org-latex-packages-alist '("" "minted" nil))
(setq org-latex-src-block-backend 'minted)

;; to make able alphabetical list in org mode
(setq org-list-allow-alphabetical t)

;; bullets instead *
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("✿" "✸" "◉" "●" "○" "☯" "☢"))) ;; Small ► • ★ ▸

;; arrow instead ...
(setq org-ellipsis " ⤵") ;;" ▾"

;; babel
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)
    (sqlite . t)
    (R . t)))

;; org-tree presentation org-tree-slide-mode! Navigate slides with C-< and C->
(use-package org-tree-slide
  :ensure t
  :custom
  (org-image-actual-width nil))

;; ORG mode CLASSES and COLORS for TASKS
(setq org-todo-keywords
      '((sequence "BACKLOG(w@/!)" "ON_WEEK(w@/!)" "TODAYYY(t)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-todo-keyword-faces
      '(
        ("BACKLOG" . (:foreground "orange" :weight bold))
        ("TODAYYY" . (:foreground "red" :weight bold))
        ("ON_WEEK" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELED" . (:foreground "gray" :weight bold))
        ))

;; ORG CAPTURE I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
  '(    ;; ... other templates
    ("l" "Link" entry (file+headline 
         "~/Gdrive/emacs_org_mode/capture.org" "Link") 
         "* LINK %^{Description} %^g
         %?
         Added: %U")
    ("j" "Journal Entry"
         entry (file+datetree "~/Gdrive/emacs_org_mode/capture.org")
         "* %?"
         :empty-lines 1)
    ("p" "Phone" entry (file+headline 
         "~/Gdrive/emacs_org_mode/capture.org" "Phone") 
         "* NUMBER %^{Description} %^g
         %?
         Added: %U")
    ("q" "Quote" entry (file+headline 
         "~/Gdrive/emacs_org_mode/capture.org" "Quote") 
         "* QUOTE %^{Description} %^g
         %?
         Added: %U")
    ("t" "Ted Talks" entry (file+headline 
         "~/Gdrive/emacs_org_mode/capture.org" "Ted") 
         "* TED %^{Description} %^g
         %?
         Added: %U")
        ;; ... other templates
    ))

;;----------------------------------------------------------------------
;; MARKDOWN MODE 
;;----------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

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

;;----------------------------------------------------------------------
;; SPECIAL PROGRAMMING TOOLS
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; multiple-cursors.el https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  )
(require 'multiple-cursors)

;;----------------------------------------------------------------------
;; indent GUIDE https://raw.githubusercontent.com/zk-phi/indent-guide/master/indent-guide.el
;;(require 'indent-guide)
;;(setq indent-guide-recursive t)

;;----------------------------------------------------------------------
(use-package smartparens
  :ensure t ;; install the package
  :hook (prog-mode text-mode markdown-mode python-mode ess-mode org-mode latex-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  )

;;----------------------------------------------------------------------
;(use-package flycheck
;  :defer t
;  :hook (lsp-mode . flycheck-mode))
 
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

;;----------------------------------------------------------------------
;; folding by indentation ;; git clone https://github.com/zenozeng/yafolding.el.git
(use-package yafolding 
  :ensure t ;; install the package
  :bind
  (("C-{" . yafolding-hide-parent-element)
     ("C-}" . yafolding-toggle-element)))

;;----------------------------------------------------------------------
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

;;----------------------------------------------------------------------
;;AUTO COMPLETE AND YASNIPPET
;;----------------------------------------------------------------------

;;----------------------------------------------------------------------
;; company-mode
;; Assegure que o use-package está instalado e inicializado primeiro (ver seção 1)
(use-package company
  :ensure t ; Garante que o company-mode seja instalado se não estiver
  :defer t  ; Otimização: carrega o company-mode quando for realmente necessário
  ;; Configurações que podem ser aplicadas antes ou durante o carregamento
  :init
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-limit 10)
  ;; Backends padrão. Ajuste conforme suas necessidades!
  ;; Se você usa LSP-mode/Eglot, eles geralmente se integram automaticamente.
  :custom
  (company-backends '(company-dabbrev-code company-files company-keywords))
  ;; Hooks para ativar globalmente ou em modos específicos
  :hook
  ;; (after-init . global-company-mode) ; Ativar globalmente após a inicialização
  ;; OU
  (prog-mode . company-mode) ; Ativar em todos os modos de programação
  ;; Bindings de teclado (opcional, company já tem defaults úteis)
  :bind
  ;;("C-<tab>" . company-complete) ; Dispara a completude manualmente
  ;; Adicione outros bindings se quiser
  )

;;----------------------------------------------------------------------
;; yasnippet
(use-package yasnippet 
  :ensure t
  :config
  (yas-global-mode 1)
  )

(defun yasnippet-snippets--fixed-indent ()
  "Set `yas-indent-line' to `fixed'."
  (set (make-local-variable 'yas-indent-line) 'fixed))

(defun yasnippet-snippets--no-indent ()
  "Set `yas-indent-line' to nil."
  (set (make-local-variable 'yas-indent-line) nil))

;;----------------------------------------------------------------------
;; add ac-sources to default ac-sources
(defun ac-LaTeX-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources)))
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)


;;----------------------------------------------------------------------
;; R ESS
;;----------------------------------------------------------------------

;; setting to work with ess and r
(use-package ess 
  :ensure t
  )
(require 'ess-site)
;(require 'ess-eldoc)
;(setq-default ess-dialect "R")
;(setq-default inferior-R-args "--no-restore-history --no-save ")

;; key SHIFT + ENTER
(eval-after-load "ess-mode"
 '(progn
   ;;(define-key ess-mode-map [(control return)] nil)
   (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)))

;; no center comments
;(setq ess-fancy-comments nil)
;(setq ess-indent-with-fancy-comments nil)
;(require 'ess)
;; No indent levels, i.e., no more identation with one '#'
(setq ess-indent-with-fancy-comments nil)
(setq ess-offset-arguments 'prev-line)

;; cancel centering comments in R ESS
;(setf (cdr (assoc 'ess-indent-with-fancy-comments ess-own-style-list)) nil)

;; if you want all help buffers to go into one frame do:
;(setq ess-help-own-frame 'one)

;;----------------------------------------------------------------------
;; REFTEX CITATION
;;----------------------------------------------------------------------

;; reftex citation pandoc http://www.unknownerror.org/opensource/jgm/pandoc/q/stackoverflow/13607156/autocomplete-pandoc-style-citations-from-a-bibtex-file-in-emacs http://tex.stackexchange.com/a/31992/5701
;; to use biber instead bibtex 
(setq TeX-parse-self t)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("/home/rafatieppo/Gdrive/bibtex/references.bib"))

;; How to solve @
(use-package citeproc
  :ensure t
  )

(eval-after-load 'reftex-vars
  '(progn 
     (setq reftex-cite-format '((?\C-m . "@%l")
                                (?\C-l . "\\cite{%l\}")
                                (?\C-o . "\\citeonline{%l\}")
                                (?\C-t . "\\citet{%l\}")
                                (?\C-p . "\\citep{%l\}")
                                ))))
(require 'citeproc)


;;----------------------------------------------------------------------
;; programming
;;----------------------------------------------------------------------

;; LATEX
;;----------------------------------------------------------------------

(use-package auctex
  :ensure t ; Garante que o pacote seja instalado se ainda não estiver
  :init
  (setq reftex-plug-into-AUCTeX t)  
  )

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;(require 'company-auctex)
;; Naveg http://piotrkazmierczak.com/2010/05/13/emacs-as-the-ultimate-latex-editor/ C-c = 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)


;; HTML
;;----------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/IndentingHtml
    (add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))

;; web-mode
(setq css-indent-offset tab-width)
(use-package web-mode
  :ensure t
  :init
  (setq web-mode-attr-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-css-indent-offset tab-width)
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-sql-indent-offset tab-width))

;; BASH
;;----------------------------------------------------------------------

(setq sh-basic-offset 2
      sh-indentation 2)
;; snippets, please
(add-hook 'sh-mode-hook 'yas-minor-mode)
;; on the fly syntax checking
(add-hook 'sh-mode-hook 'flycheck-mode)

;; PYTHON CONFIGURATION
;;----------------------------------------------------------------------

;; --- Elpy Configuration ---
;; using python-mode

;; --- Conda ---
;; https://www.reddit.com/r/emacs/comments/hkshob/save_correct_condaenv_for_project/fwxty9v
;;
;; 1. Set `conda-project-env-name' as a directory local variable. (With
;;    projectile you could use the `projectile-edit-dir-locals'
;;    command.)
;; 2. Use `conda-env-activate-for-buffer' to activate the environment
;;    set. (Or enable `conda-env-autoactivate-mode' to automatically
;;    activate it.)
;; A conda environment manager, assuming the use of Anaconda and the
;; `conda` tool. See https://github.com/necaris/conda.el for more
;; details. https://melpa.org/#/conda.
(use-package conda
  :ensure t
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  :init
  ;; (message "HERE conda init")
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/envs"))
  :config
  ;; (message "HERE conda config")
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  ;; (conda-env-activate 'getenv "CONDA_DEFAULT_ENV")
  (conda-env-autoactivate-mode t)
  )

(use-package company-anaconda
  :ensure t
  )

;; Code navigation, documentation lookup and completion for Python.
;; https://github.com/pythonic-emacs/anaconda-mode
;; https://melpa.org/#/anaconda-mode
(use-package anaconda-mode
  :ensure t
  :bind (:map python-mode-map ("C-;" . company-anaconda))
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  ;; :init
  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
)

;; --- Eglot Configuration and Integration ---
(use-package eglot
  :init
  ;; 'pylsp' (Python Language Server) is a common and recommended choice.
  ;; Make sure it's installed in your py environment: pip install "python-lsp-server[all]"
  ;(add-to-list 'eglot-server-programs '(python-mode "pylsp"))

  ;; If you prefer pyright,Make sure to install it
  ;(add-to-list 'eglot-server-programs '(python-mode "pyright-langserver" "--node-ipc"))

  ;; Optional: Automatically activate Eglot in Python buffers
  :hook
  ;(python-mode . eglot-mode)
   (python-mode . eglot-ensure)
  ;(elpy-mode . eglot-ensure)

  :config
  ;; Optional Eglot settings
  (setq eglot-autoshutdown t) ; Shut down the server when no associated buffers
  (setq eglot-connect-timeout 60) ; Increase timeout for LSP connection
  ;; (setq eglot-sync-stdout t) ; Useful for LSP server debugging
  )

;;https://gitlab.com/skybert/my-little-friends/-/blob/master/emacs/.emacs
;(use-package eglot
;  :ensure t
;  :hook
;  ((python-mode . eglot-ensure))
;  )

;; https://emacs-lsp.github.io/lsp-pyright/ ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/ ;; https://www.mattduck.com/lsp-python-getting-started.html
;; FOR lsp-pyright
;(use-package lsp-pyright
;  :ensure t
;  :hook (python-mode . (lambda ()
;                          (require 'lsp-pyright)
;                          (lsp))))  ;lsp or lsp-deferred 

;; YAML
;;----------------------------------------------------------------------
(use-package yaml-mode
  :ensure t ; Ensure the yaml-mode package is installed
  :mode ("\\.yaml\\'" "\\.yml\\'") ; Associate this mode with these file extensions
  :hook (yaml-mode . (lambda ()
                       (setq indent-tabs-mode nil) ; Optional: Ensure YAML uses spaces for indentation
                       (setq tab-width 4) ; Optional: Set tab width to 2 spaces
                       (setq completion-styles '(initials substring)) ; Optional: Improve completion for YAML
                       ))
  )

;; PHP CONFIGURATION
;;----------------------------------------------------------------------
(use-package lsp-mode
 :ensure t
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

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; lsp-mode CONFIGURATION
;;----------------------------------------------------------------------

;; to stop messages in minibuffer
;(setq lsp-headerline-breadcrumb-enable nil)

;; if you are helm user
; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

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
;;----------------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ; helm or ivy
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))


;; MAGIT
;;----------------------------------------------------------------------

(use-package magit
  :ensure t)

;;----------------------------------------------------------------------
;;FUNCOES functions
;;----------------------------------------------------------------------
;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/lisp")
(load "/home/rafatieppo/.emacs.d/lisp/functions")
;(require 'functions)

;;----------------------------------------------------------------------
;; THEMES - SEVERAL SCHEMES
;;----------------------------------------------------------------------
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
;(require 'doom-themes)
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
;(require 'timu-rouge-theme)
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
   '("aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09"
     "356b7dc07192e3fdc0b131d80b48b3a5d7a8be291561abbbc601072d601b2f23"
     "846ef3695c42d50347884515f98cc359a7a61b82a8d0c168df0f688cf54bf089"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

