;;=============================================================================
;; Arquivo de configuração do Emacs (>=24.3.1) por Rafael Tieppo.
;; Este arquivo encontra-se disponível em
;; A grande maioria do conteúdo aqui disponível foi obtido/inspirado a
;; partir de consultas na internet. Encaminhe dúvidas, problemas e/ou
;; sugestões como um issue no diretório GitHub desse arquivo.
;;=============================================================================

;;===========================================================================
;;TO INSTALL FROM MELPA
;;===========================================================================
;;-----------------------------------------------------------------------------
  
(add-to-list 'load-path "/home/rafatieppo/.emacs.d/")
(load "package")
(require 'package)
;;(add-to-list 'package-archives
;;    '("marmalade" .
;;      "http://marmalade-repo.org/packages/"))
(package-initialize) 

(setq package-enable-at-startup nil)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ;;Org-mode's repository
;;-----------------------------------------------------------------------------


;;===========================================================================
;;EMACS auto install packs GREAT - active only in the first session
;;===========================================================================

;; list the packages you want
;(setq package-list '(ace-jump-mode ac-math auctex auto-complete elpy ess ess-R-data-view ess-R-object-popup flx flx-ido flycheck highlight-symbol ido-hacks ido-vertical-mode indent-guide jedi markdown-mode multiple-cursors polymode popup powerline py-autopep8 smartparens smex yafolding yasnippet))

;; list the repositories containing them
;(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
;                         ("gnu" . "http://elpa.gnu.org/packages/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; activate all the packages (in particular autoloads)
;(package-initialize)

;; fetch the list of packages available 
;(unless package-archive-contents
;  (package-refresh-contents))

;; install the missing packages
;(dolist (package package-list)
;  (unless (package-installed-p package)
;    (package-install package)))
;-----------------------------------------------------------------------------

;;===========================================================================
;;ORG MODE
;;===========================================================================

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; arquivo para o org agendasorg-agenda
(setq org-agenda-files '("/home/rafatieppo/Dropbox/EMACS_ORG_MODE/RAFA.org"))



;;-----------------------------------------------------------------------------
;; ORG mode CLASSES and COLORS for TASKS
(setq org-todo-keywords
       '((sequence "TODO(t)" "RUNN(w@/!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))


     (setq org-todo-keyword-faces
           '(
             ("TODO" . (:foreground "red" :weight bold))
             ("RUNN" . (:foreground "yellow" :weight bold))
             ("WAIT" . (:foreground "orange" :weight bold))
             ("DONE" . (:foreground "green" :weight bold))
             ))



;;-----------------------------------------------------------------------------
;; ORG CAPTURE

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
  '(    ;; ... other templates

    ("l" "Link" entry (file+headline 
         "~/Dropbox/EMACS_ORG_MODE/CAPTURE.org" "Link") 
         "* LINK %^{Description} %^g
         %?
         Added: %U")

    ("j" "Journal Entry"
         entry (file+datetree "~/Dropbox/EMACS_ORG_MODE/CAPTURE.org")
         "* %?"
         :empty-lines 1)

    ("p" "Phone" entry (file+headline 
         "~/Dropbox/EMACS_ORG_MODE/CAPTURE.org" "Phone") 
         "* NUMBER %^{Description} %^g
         %?
         Added: %U")

    ("q" "Quote" entry (file+headline 
         "~/Dropbox/EMACS_ORG_MODE/CAPTURE.org" "Quote") 
         "* QUOTE %^{Description} %^g
         %?
         Added: %U")

    ("t" "Ted Talks" entry (file+headline 
         "~/Dropbox/EMACS_ORG_MODE/CAPTURE.org" "Ted") 
         "* TED %^{Description} %^g
         %?
         Added: %U")


        ;; ... other templates
    ))

;;-----------------------------------------------------------------------------
;; Chronometer Task
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;-----------------------------------------------------------------------------
;; ORG MONTH REPORT

(defun my/org-review-month (start-date)
  "Review the month's clocked tasks and time."
  (interactive (list (org-read-date)))
  ;; Set to the beginning of the month
  (setq start-date (concat (substring start-date 0 8) "01"))
  (let ((org-agenda-show-log t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)))
    (org-agenda-list nil start-date 'month)))
  
;;-----------------------------------------------------------------------------
;; ORG REPORTING TIME BY DAY
;; http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/

(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports."
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
        (insert "\n\n"
                (format-time-string (car org-time-stamp-formats)
                                    (seconds-to-time start))
                "----------------\n")
        (org-dblock-write:clocktable
         (plist-put
          (plist-put
           params
           :tstart
           (format-time-string (car org-time-stamp-formats)
                               (seconds-to-time start)))
          :tend
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time end))))
        (setq start (+ 86400 start))))))


;;----------------------------------------------------------------------
;; POWERLINES
;;----------------------------------------------------------------------
;;https://github.com/milkypostman/powerline/
(require 'powerline)
(powerline-default-theme)
;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Provide to run R code
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (clojure . t)
   (python . t)
   (R . t)))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Provide Highlight code in SRC

(setq org-src-fontify-natively t)
;;-----------------------------------------------------------------------------

;;===========================================================================
;;STANDARD SETTINGS
;;===========================================================================
;;---------------------------------------------------------------------------
; There is no welcome windows
;; http://blog.droidzone.in/2012/05/22/remove-startup-split-screen-in-emacs/
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;New buffer horizontal or vertical
;https://www.emacswiki.org/emacs/HorizontalSplitting
;(setq split-width-threshold 9999)
(setq split-width-threshold 0)
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Tipo e tamanho da fonte do editor.
;(set-default-font "monofur-13")
;(set-default-font "Tex Gyre Adventor-11")
;(set-default-font "Anonymous Pro-14.5")
(set-default-font "Menlo-15")
;(custom-set-faces
; '(default ((t (:family "Anonymous Pro" :foundry "unknown" :slant normal :weight normal :height 240 :width normal)))))
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Space between lines and between line numbers and text (like margin)
(setq-default line-spacing 3) 
(setq-default left-fringe-width  3)
;(setq-default right-fringe-width  0)
;(set-face-attribute 'fringe nil :background "black")
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; ativa DEAD keys quando usar LATEX
;; Para funcionar acentuação no Sony Vaio.
(require 'iso-transl)
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; numeração das linhas na margem esquerda
(global-linum-mode 1)
;;---------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Mostra posição do cursor em relação a margem esquerda.
(setq column-number-mode t)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Show file name in title bar
;; http://www.thetechrepo.com/main-articles/549
(setq frame-title-format "%b - Emacs")
;;---------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Quebra de linhas ao exceder largura do texto (padrão é 72
;; caracteres).
(setq-default fill-column 72)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Não quebrar linhas, útil para tabelas longas
(setq-default truncate-lines t)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Ativa o auto-fill-mode para fazer quebra automática de linhas.
(setq-default auto-fill-function 'do-auto-fill)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Modo de linhas de tela (screen lines) e não lógicas (logical lines).
(visual-line-mode 1)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Desativa o auto salvar e auto backup.
(setq auto-save-default nil) ;; Para o #autosave#.
(setq make-backup-files nil) ;; Para o backup~.
;;---------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Usa espaços ao ínves de tabs para indentar.
;; http://xenon.stanford.edu/~manku/dotemacs.html
(setq-default indent-tabs-mode nil)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; inicia Emacs com ctrl-{zxcv} abilitado para desf/recor/cop/colar
(cua-mode t)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; stop cursor blinking
(blink-cursor-mode 0)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; removes menu bar
(menu-bar-mode -1)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; removes menu bar
(tool-bar-mode -1)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; tecla SHIFT + ENTER
(eval-after-load "ess-mode"
 '(progn
   (define-key ess-mode-map [(control return)] nil)
   (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step))
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Highlight matching pairs.
(show-paren-mode 1)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;;How to have emacs highlight text selections?	
;;	(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Speedbar embed
;(require 'sr-speedbar)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; SPECIAL PROGRAMMING TOOLS
;;===========================================================================

;;-----------------------------------------------------------------------------
;; multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
;; continuous lines
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ace-jump-mode.el --- a quick cursor location minor mode for emacs -*- coding: utf-8-unix -*-
;; https://github.com/winterTTr/ace-jump-mode
;; ace jump mode major function

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; enable a more powerful jump back function from ace jump mode

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Ido Search FILE

(ido-mode t)
;(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq ido-file-extensions-order '(".md" ".R" ".Rmd" ".csv" ".txt" ".org" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Improving Ido Search FILE

;;http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/
;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/lisp")
(require 'smex)
(require 'ido-vertical-mode)
(require 'ido-hacks)
(require 'flx)
(require 'flx-ido)

(ido-mode 1)
(require 'ido-hacks nil t)
(if (commandp 'ido-vertical-mode) 
    (progn
      (ido-vertical-mode 1)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))
(if (commandp 'smex)
    (global-set-key (kbd "M-x") 'smex))
(if (commandp 'flx-ido-mode)
    (flx-ido-mode 1))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; FUNCTION HIGHLIGHTS LISP

    (require 'highlight-symbol)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; INDENT GUIDE
;; https://raw.githubusercontent.com/zk-phi/indent-guide/master/indent-guide.el
(require 'indent-guide)
(setq indent-guide-recursive t)
;;-----------------------------------------------------------------------------

;;===========================================================================
;;AUTO COMLETE
;;===========================================================================

(require 'yasnippet)
(yas-global-mode 1)

;;-----------------------------------------------------------------------------
;; AUTO COMPLETE FUNCTION
;;aciona AUTO-COMPLETE
;;-----------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/AutoCompleteSources
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; stop (auto-complete-mode) from being called in python
;; https://stackoverflow.com/questions/24814988/emacs-disable-auto-complete-in-python-mode
;(defadvice auto-complete-mode (around disable-auto-complete-for-python)
;  (unless (eq major-mode 'python-mode) ad-do-it))

;(ad-activate 'auto-complete-mode)


;; macro .el, not necessary
;; auto-complete for latex 
;;(require 'auto-complete-auctex)

(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

 (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
   (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                 ac-sources))
   )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
;(global-auto-complete-mode t)
 
(setq ac-math-unicode-in-math-p t)

(ac-flyspell-workaround) ;; to make flyspell works with auto-complete

;; to use biber instead bibtex
;; https://tex.stackexchange.com/questions/154751/biblatex-with-biber-configuring-my-editor-to-avoid-undefined-citations/154753#154753
(setq TeX-parse-self t)
;;----------------------------------------------------------------------
;; To activate ESS auto-complete for R.
;;----------------------------------------------------------------------

(setq ess-use-auto-complete 'script-only)

;;----------------------------------------------------------------------
;; CHANGE 'ac-complete FROM ENTER to TAB.
;;----------------------------------------------------------------------
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map "\t" 'ac-complete)

;;----------------------------------------------------------------------
;; DROPDOWN DELAY
;;----------------------------------------------------------------------
(setq ac-auto-show-menu    0.2)

;;----------------------------------------------------------------------
;; auto complete markdown
;; http://wiki.dreamrunner.org/public_html/Emacs/markdown.html
;;----------------------------------------------------------------------

;(add-hook 'markdown-mode-hook
;          '(lambda ()
;             (auto-complete-mode t)
;             (local-unset-key [tab])
;             (setq-local yas-fallback-behavior '(apply auto-complete))))

;;---------------------------------------------------------------------------
; Automatic brackets, etc
;;----------------------------------------------------------------------

;; ref: http://www.emacswiki.org/emacs/ESSAutoParens
;; enable skeleton-pair insert globally
;(setq skeleton-pair-on-word t)
;(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)
;;;; make electric-pair-mode work on more brackets
;(electric-pair-mode 1)	

;;----------------------------------------------------------------------
;; Smart Parenthesis.
;; https://github.com/Fuco1/smartparens
;;----------------------------------------------------------------------

(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)

;;-----------------------------------------------------------------------------
;; FOLDING BY INDENTATION
;; Folding code blocks based on indentation.
;; git clone https://github.com/zenozeng/yafolding.el.git
;;-----------------------------------------------------------------------------

(require 'yafolding)

(global-set-key [?\C-{] #'yafolding-hide-parent-element)
(global-set-key [?\C-}] #'yafolding-toggle-element)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ADD HIGHLIGHT FOR CERTAIN KEYWORDS
;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-09/txtb5ChQJCDny.txt
;; http://emacs.1067599.n5.nabble.com/Adding-keywords-for-font-lock-experts-td95645.html

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
            polymode-mode
            python-mode
            markdown-mode
            TeX-mode)) 
  (font-lock-add-keywords
   mode 
   '(("\\<\\(COMMENT\\|DONE\\|TODO\\|STOP\\|IMPORTANT\\|NOTE\\|OBS\\|ATTENTION\\|REVIEW\\)" 
      0 'font-lock-warning-face t) 
     ("\\<\\(BUG\\|WARNING\\|DANGER\\|FIXME\\)" 
      0 'special-words t)))
  ) 
;;-----------------------------------------------------------------------------
 

;;===========================================================================
;; SETTING TO WORK WITH ESS and R
;;===========================================================================
;;---------------------------------------------------------------------------
;; IF YOU GET ERROR TO LOAD ESS: The immediate fix is to delete <user-emacs-directory>/elpa/archives/melpa/archive-contents; it will be rebuilt on the next package-initialize.
;; faz com que apareceça os argumentos das funções do R no minibuffer
(require 'ess-site)
(setq-default ess-dialect "R")
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; If you want all help buffers to go into one frame do:
(setq ess-help-own-frame 'one)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ESS - HIGHLIGHTS ON PROGRAMING CODES
;;-----------------------------------------------------------------------------
 (setq ess-R-font-lock-keywords
         '((ess-R-fl-keyword:modifiers . t) ; default
           (ess-R-fl-keyword:fun-defs . t) ; default
           (ess-R-fl-keyword:keywords . t) ; default
           (ess-R-fl-keyword:assign-ops . t) ; default
           (ess-R-fl-keyword:constants . t) ; default
           (ess-fl-keyword:fun-calls . t)
           (ess-fl-keyword:numbers . t)  ;;se ativar fica muita colorido
           (ess-fl-keyword:operators . nil)
           (ess-fl-keyword:delimiters . t) ;;se ativar fica muita colorido
           (ess-fl-keyword:= . nil) ;;se ativar fica muita colorido
           (ess-R-fl-keyword:F&T . t)))

   (setq inferior-R-font-lock-keywords
         '((ess-S-fl-keyword:prompt . t) ; default
           (ess-R-fl-keyword:messages . t) ; default
           (ess-R-fl-keyword:modifiers . nil) ; default
           (ess-R-fl-keyword:fun-defs . nil) ; default
           (ess-R-fl-keyword:keywords . t) ; default
           (ess-R-fl-keyword:assign-ops . nil) ; default
           (ess-R-fl-keyword:constants . t) ; default
           (ess-fl-keyword:matrix-labels . t) ; default
           (ess-fl-keyword:fun-calls . t)
;;           (ess-fl-keyword:numbers . nil)
;;           (ess-fl-keyword:operators . nil)
;;           (ess-fl-keyword:delimiters . nil)
;;           (ess-fl-keyword:= . t)
           (ess-R-fl-keyword:F&T . t)))
;;-----------------------------------------------------------------------------

;;===========================================================================
;;FUNCOES functions
;;===========================================================================

;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/lisp")
(load "/home/rafatieppo/.emacs.d/lisp/functions")
;(require 'functions)


;;===========================================================================
;;MARKDOWN MODE 
;;===========================================================================

;;-----------------------------------------------------------------------------
;; ORG MOMDE MINOR mode markdown
;; http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
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
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; If using markdown-mode yasnippets’s TAB completion doesn’t work, it’s just because TAB key is bind to markdown-cycle function
;; http://wiki.dreamrunner.org/public_html/Emacs/markdown.html
(add-hook 'markdown-mode-hook
          '(lambda ()
             (auto-complete-mode t)
             (local-unset-key [tab])
             (setq-local yas-fallback-behavior '(apply auto-complete))))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; MARKDOWN enable MATH Desativei pq fica muito colorido e confunde
;;http://jblevins.org/projects/markdown-mode/
(setq markdown-enable-math t)

;;===========================================================================
;; MARKDOWN EXTENSIONS.
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)
;;===========================================================================

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org-struct minor mode active in markdown mode.
(add-hook 'markdown-mode-hook 'turn-on-orgstruct)
(add-hook 'markdown-mode-hook 'turn-on-orgstruct++)

;;===========================================================================
;; POLY-MODE MAKDOWN ;; (DEVE ESTAR ANTES DO SUPORTE PARA LATEX.)
;;===========================================================================

;; PASSOS
;; Suporte para R+MarkDown (requer emacs >= 24.3.1).
;; Obter polymode da origem (em constante modificação).
;; http://stackoverflow.com/questions/16567348/knitr-markdown-highlighting-in-emacs
;; cd ~/Downloads/
;; git clone https://github.com/vitoshka/polymode.git
;; cd polymode
;; mkdir ~/.emacs.d/polymode/
;; cp -v *.el ~/.emacs.d/polymode/
;; cp -rv ./modes/ ~/.emacs.d/polymode/
;; cd .. && rm -rf polymode
;; Adicionar os diretórios com os modos.

;(setq load-path
;(append '("/home/rafatieppo/.emacs.d/polymode/"
;"/home/rafatieppo/.emacs.d/polymode/modes")
;load-path))

;; Chama os modos.

;(require 'poly-R)
;(require 'poly-markdown)
;(require 'poly-noweb)

(autoload 'poly-markdown-mode "poly-markdown-mode"
  "Major mode for editing R-Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rpres\\'" . poly-markdown-mode))
;;-----------------------------------------------------------------------------

;;===========================================================================
;; REFTEX CITATION PANDOC
;; http://www.unknownerror.org/opensource/jgm/pandoc/q/stackoverflow/13607156/autocomplete-pandoc-style-citations-from-a-bibtex-file-in-emacs
;; http://tex.stackexchange.com/a/31992/5701
;;===========================================================================

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("/home/rafatieppo/Dropbox/BIBTEX/REFERENCES.bib"))

;; How to solve @
(eval-after-load 'reftex-vars
  '(progn 
     (setq reftex-cite-format '((?\C-m . "@%l")
                                (?\C-l . "\\cite{%l\}")
                                (?\C-o . "\\citeonline{%l\}")
                                (?\C-t . "\\citet{%l\}")
                                (?\C-p . "\\citep{%l\}")
                                ))))
     
;; I changed the code, before "[@%l]" cite between brackets
;;-----------------------------------------------------------------------------

;;===========================================================================
;; LATEX
;;===========================================================================
;;-----------------------------------------------------------------------------
;; Modo matemático para LaTex (Math no menu com atalhos para símbolos,
;; etc).
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Suporte do refTex para navegar por grandes documentos (Ref no menu,
;; navegação, sumário).
;; http://piotrkazmierczak.com/2010/05/13/emacs-as-the-ultimate-latex-editor/
;; Para ativar: C-c =  it means CTRL + c + = 

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;;atalhos UTEIS
;;;;;automatic formatting of a section: C-c C-q C-s;
;;;;;section preview: C-c C-p C-s; (see the screenshot on the right)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; CONFIGURACOES AVANCADAS AUCTEX
;;===========================================================================
;;http://tex.stackexchange.com/questions/161797/how-to-configure-emacs-and-auctex-to-perform-forward-and-inverse-search

 ;; '(TeX-source-correlate-method (quote synctex))
 ;; '(TeX-source-correlate-mode t)
 ;; '(TeX-source-correlate-start-server t)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; HTML
;;===========================================================================
;;-----------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/IndentingHtml
    (add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))
;;-----------------------------------------------------------------------------

;;===========================================================================
;; PYTHON CONFIGURATION
;;===========================================================================
;; Anaconda
;(add-hook 'python-mode-hook 'anaconda-mode)
;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Elpy
(elpy-enable)
;(elpy-use-ipython)

;; Flycheck
;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Autopep8
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;-----------------------------------------------------------------------------
;; JEDI

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)                 ; optional
;-----------------------------------------------------------------------------
; FIX to EMACS 25.1


;;-----------------------------------------------------------------------------
;; ALT ENTER to send line

(defun my-python-line ()
 (interactive)
  (save-excursion
  (setq the_script_buffer (format (buffer-name)))
  (end-of-line)
  (kill-region (point) (progn (back-to-indentation) (point)))
  ;(setq the_py_buffer (format "*Python[%s]*" (buffer-file-name)))
  (setq the_py_buffer "*Python*")
  (switch-to-buffer-other-window  the_py_buffer)
  (goto-char (buffer-end 1))
  (yank)
  (comint-send-input)
  (switch-to-buffer-other-window the_script_buffer)
  (yank))
  (beginning-of-line) ;; or (end-of-line)
  (next-line)
)

(global-set-key (kbd "M-RET") 'my-python-line) ; Enter/Return key

;;-----------------------------------------------------------------------------
;; ALT / to send region

(defun python-shell-send-region-or-line nil
  "Sends from python-mode buffer to a python shell, intelligently."
  (interactive)
  (cond ((region-active-p)
     (setq deactivate-mark t)
     (python-shell-send-region (region-beginning) (region-end))
     (python-nav-forward-statement)
 ) (t (elpy-shell-send-current-statement))))
;elpy-shell-send-region

;https://emacs.stackexchange.com/questions/27674/make-elpy-shell-send-more-intelligent

(global-set-key (kbd "M-/") 'python-shell-send-region-or-line) ; alt + /


;;-----------------------------------------------------------------------------
;; Set Python3 interpreter

;(setq python-shell-interpreter "/usr/bin/python3")
(setq python-shell-interpreter "/home/rafatieppo/anaconda3/bin/python3")


;;===========================================================================
;; THEMES - SEVERAL SCHEMES
;;===========================================================================
;;-----------------------------------------------------------------------------
;; THEMES from: http://emacsthemes.caisah.info/
;; https://github.com/owainlewis/emacs-color-themes
;; themes from: http://emacsthemes.caisah.info/
;; https://github.com/juba/color-theme-tangotango/blob/master/tangotango-theme.el
;; http://emacsthemes.com
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Solarized

;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/themess/emacs-color-theme-solarized-master")
;(require 'solarized-dark-theme)
;(require 'solarized-definitions)
;(require 'solarized-theme)
;(require 'color-theme-solarized)

;(custom-set-variables '(solarized-termcolors 256))
;(custom-set-variables '(solarized-contrast 'high))
;(custom-set-variables '(solarized-visibility 'high))

;;-----------------------------------------------------------------------------
;; Solarized https://github.com/bbatsov/solarized-emacs

;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/themess/solarized")
;(require 'solarized-dark-theme)

;; options
;; make the fringe stand out from the background
;(setq solarized-distinct-fringe-background t)
;; Don't change the font for some headings and titles
;(setq solarized-use-variable-pitch nil)
;; make the modeline high contrast
;(setq solarized-high-contrast-mode-line t)
;; Use less bolding
;(setq solarized-use-less-bold t)
;; Use more italics
;(setq solarized-use-more-italic t)
;; Use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)
;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)
;; Avoid all font-size changes
;(setq solarized-height-minus-1 1.0)
;(setq solarized-height-plus-1 1.0)
;(setq solarized-height-plus-2 1.0)
;(setq solarized-height-plus-3 1.0)
;(setq solarized-height-plus-4 1.0)

(add-to-list 'load-path "/home/rafatieppo/.emacs.d/themess")

;;(require 'monokai-theme) ;; load first to improve ORG visualization
;;(require 'Amelie-theme)
;;(require 'ample-zen-theme)
;;(require 'assemblage-theme)
;;(require 'atom-one-dark-theme)
;;(require 'deep-thought-theme)
;(require 'challenger-deep-theme.el)
;;(require 'dracula-themes)
;;(require 'erosiond-theme)
;;(require 'fogus-theme)
;;(require 'granger-theme)
;;(require 'hickey-theme)
;;(require 'junio-theme)
;;(require 'material-light-theme)
(require 'material-theme)
;;(require 'moe-dark-theme)
;;(require 'molokai-theme)
;;(require 'odersky-theme)
;;(require 'seti-theme)
;;(require 'soothe-theme)
;;(require 'spolsky-theme)
;;(require 'tangotango-theme)
;;(require 'ujelly-theme)
;;(require 'underwater-theme)
;;(require 'wilson-theme)
;;(require 'zenburn-theme)

;;===========================================================================
;;TEMA VEM PADRAO EMACS
;;===========================================================================

;;---------------------------------------------------------------------------
;;Padrao do EMACS cursor linha; Must be after of THEME to do not overlayer
;;http://stackoverflow.com/questions/2718189/emacshighlight-the-current-line-by-underline-it/2718543#2718543
(global-hl-line-mode 1)
;; Underline in current line
;(set-face-attribute hl-line-face nil :underline t)
;;(set-face-background hl-line-face "#2F2F2F") ;;MONOKAI
;;---------------------------------------------------------------------------
;;===========================================================================



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yafolding sr-speedbar smex smartparens py-autopep8 powerline polymode multiple-cursors markdown-toc jedi indent-guide ido-vertical-mode ido-hacks highlight-symbol flycheck flx-ido ess-R-object-popup ess-R-data-view elpy auctex ace-jump-mode ac-math))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
