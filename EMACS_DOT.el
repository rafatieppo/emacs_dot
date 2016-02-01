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
  
(add-to-list 'load-path "~/.emacs.d/")
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
;;ORG MODE
;;===========================================================================

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; arquivo para o org agendasorg-agenda
(setq org-agenda-files '("/home/rafatieppo/Dropbox/EMACS_ORG_MODE/RAFA.org"))


;;-----------------------------------------------------------------------------
;; ORG mode for Android
;;http://stackoverflow.com/questions/11822353/how-to-make-org-mobile-work-in-android
;; http://blog.gabrielsaldana.org/mobileorg-for-android-setup-and-workflow/

;; where all your org files will be stored
(setq org-directory "/home/rafatieppo/Dropbox/EMACS_ORG_MODE")

(setq org-mobile-directory "/home/rafatieppo/Dropbox/MOBILEORG")

(setq org-mobile-inbox-for-pull "/home/rafatieppo/Dropbox/EMACS_ORG_MODE/mobile.org")

;;(setq default-buffer-file-coding-system 'utf-8)

(setq org-mobile-files '("/home/rafatieppo/Dropbox/EMACS_ORG_MODE/RAFA.org"))
(setq org-mobile-agendas '("a"))

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

;;-----------------------------------------------------------------------------
;; ORG SHORCUT
;;; Quick inserts via <s TAB and similar 

(setq org-structure-template-alist '(("s" "#+BEGIN_SRC ?\n\n#+END_SRC")
                                     ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
                                     ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE")
                                     ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE")
                                     ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM")
                                     ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER")
                                     ("l" "#+BEGIN_LaTeX\n?\n#+END_LaTeX")
                                     ("L" "#+LaTeX: ")
                                     ("h" "#+BEGIN_HTML\n?\n#+END_HTML")
                                     ("H" "#+HTML: ")
                                     ("a" "#+BEGIN_ASCII\n?\n#+END_ASCII")
                                     ("A" "#+ASCII: ")
                                     ("i" "#+INDEX: ?")
                                     ("I" "#+INCLUDE: %file ?")
                                     ("x" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")))

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
;; Provide Highlight code in SRC

(setq org-src-fontify-natively t)

;;-----------------------------------------------------------------------------
;; Provide Highlight in LATEX and PDF
;; http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles

;;(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;;===========================================================================
;;STANDARD SETTINGS
;;===========================================================================
;;---------------------------------------------------------------------------
; Abre o emacs sem a janela de boas vindas.
;; http://blog.droidzone.in/2012/05/22/remove-startup-split-screen-in-emacs/
(setq inhibit-startup-screen t)
(add-hook 'emacs-startup-hook 'delete-other-windows)[/code]
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Tipo e tamanho da fonte do editor.
;(set-default-font "monofur-13")
(custom-set-faces
 '(default ((t (:family "Anonymous Pro" :foundry "unknown" :slant normal :weight normal :height 138 :width normal)))))
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

;;-----------------------------------------------------------------------------
;; Remove white espace at end when save buffer.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
;; (require 'whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style
      '(face lines-tail trailing spaces tabs empty))
(global-whitespace-mode +1)
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

;;---------------------------------------------------------------------------
;; tecla SHIFT + ENTER
(eval-after-load "ess-mode"
 '(progn
   (define-key ess-mode-map [(control return)] nil)
   (define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step))
)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
; Automatic brackets, etc
;; ref: http://www.emacswiki.org/emacs/ESSAutoParens
;; enable skeleton-pair insert globally
(setq skeleton-pair t)
;;(setq skeleton-pair-on-word t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;;;; make electric-pair-mode work on more brackets
;;(electric-pair-mode 1)	
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; realçador de pareamento de parenteses, chaves, colchetes, aspas...
(show-paren-mode 1)
(global-font-lock-mode t) ; turn on syntax highlighting
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;;How to have emacs highlight text selections?	
;;	(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Speedbar embed
(require 'sr-speedbar)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; TECLAS DE ATALHO
;;===========================================================================
;;----------------------------------------------------------------------------- 
;; Define C-TAB para mudar o cursor de janelas (buffers ativos).
(global-set-key [(control tab)] 'other-window)

;; Define C-page down e C-page up para mover entre buffers.
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
;;----------------------------------------------------------------------------- 

;;----------------------------------------------------------------------------- 
;; Define C-F4 para fechar um buffer.
(define-key global-map [(control f4)] 'kill-buffer)
;;----------------------------------------------------------------------------- 

;;----------------------------------------------------------------------------- 
;; Define C-F1 para (des)ativar o flyspell.
(define-key global-map [(control f1)] 'flyspell-mode)
;;----------------------------------------------------------------------------- 

;;----------------------------------------------------------------------------- 
;; Para passar o corretor ortográfico em uma região.
(define-key global-map [(control f2)] 'flyspell-region)
;;----------------------------------------------------------------------------- 

;;----------------------------------------------------------------------------- 
;; Define C-- para fazer linha com 60 sinais de -.
(global-set-key [?\C--] (kbd "C-u 6 0 -"))
;;-----------------------------------------------------------------------------

;;----------------------------------------------------------------------------- 
;; Define C-= para fazer linha com 60 sinais de -.
(global-set-key [?\C--] (kbd "C-u 6 0 ="))
;;-----------------------------------------------------------------------------

;;----------------------------------------------------------------------------- 
;; Define F11 e F12 para trocar entre modo com e sem menus.
;; (global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f11>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<f12>") 'toggle-tool-bar-mode-from-frame)
;;----------------------------------------------------------------------------- 

;;===========================================================================
;; SPECIAL PROGRAMMING TOOLS
;;===========================================================================

;;-----------------------------------------------------------------------------
;; multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

;; continuous lines
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-. C-c") 'mc/edit-lines)

;; not based on continuous lines
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-; C-<") 'mc/mark-all-like-this)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ace-jump-mode.el --- a quick cursor location minor mode for emacs -*- coding: utf-8-unix -*-
;; https://github.com/winterTTr/ace-jump-mode
;; ace jump mode major function
;; 
;;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Ido Search FILE

(ido-mode t)
;(setq ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq ido-file-extensions-order '(".md" ".R" ".Rmd" ".csv" ".txt" ".org" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;;---------------------------------------------------------------------------
;; Improving Ido Search FILE

;;http://sachachua.com/blog/2014/03/emacs-basics-call-commands-name-m-x-tips-better-completion-using-ido-helm/
;(add-to-list 'load-path "/home/rafatieppo/.emacs.d")
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
;;Add the following to your .emacs file:
;    (add-to-list 'load-path "/home/rafatieppo/.emacs.d")

    (require 'highlight-symbol)
    (global-set-key [(control f3)] 'highlight-symbol-at-point)
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; AUTO COMPLETE FUNCTION
;;aciona AUTO-COMPLETE
;(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
;;If you want AC only in your ESS buffers do:`Funciona`
(setq ess-use-auto-complete 'script-only)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; SETTING TO WORK WITH ESS and R
;;===========================================================================
;;---------------------------------------------------------------------------
;; faz com que apareceça os argumentos das funções do R no minibuffer
(require 'ess-site)
(setq-default ess-dialect "R")
(require 'ess-eldoc)

;(require 'ess-eldoc)
;(setq-default ess-dialect "R")
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
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
(load "~/.emacs.d/lisp/functions")
;(require 'functions)

;;===========================================================================
;; POLY-MODE MAKDOWN ;; (DEVE ESTAR ANTES DO SUPORTE PARA LATEX.)
;;===========================================================================

;; Suporte para MarkDown.
;; (DEVE ESTAR ANTES DO SUPORTE PARA LATEX.)
;; O markdown-mode.el vem com a instalação do emacs-goodies-el.
;; sudo apt-get install emacs-goodies-el
;; Inicia no modo para markdown para arquivos *.md e *.markdown.

;; http://jblevins.org/projects/markdown-mode/

(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;-----------------------------------------------------------------------------

;; PASSOS
;; Suporte para R+MarkDown (requer emacs >= 24.3.1).
;; (DEVE ESTAR ANTES DO SUPORTE PARA LATEX.)
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

(setq load-path
(append '("/home/rafatieppo/.emacs.d/polymode/"
"/home/rafatieppo/.emacs.d/polymode/modes")
load-path))

;; Chama os modos.
(require 'poly-R)
(require 'poly-markdown)
(require 'poly-noweb)
(autoload 'poly-markdown-mode "poly-markdown-mode"
"Major mode for editing R-Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))


;;/home/rafatieppo/.emacs.d/elpa/polymode-20141204.2346/

;;-----------------------------------------------------------------------------

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
;; MARKDOWN enable MATH Desativei pq fica muito colorido e confunde
;;http://jblevins.org/projects/markdown-mode/
(setq markdown-enable-math t)


;;-----------------------------------------------------------------------------
;; MARKDOWN VIEWER LIVE
;; https://github.com/ancane/markdown-preview-mode
;;(add-to-list 'load-path "~/.emacs.d/")
;;(require 'websocket)

;;-----------------------------------------------------------------------------
;; MARKDOWN AND REFTEX CITATION PANDOC
;; http://www.unknownerror.org/opensource/jgm/pandoc/q/stackoverflow/13607156/autocomplete-pandoc-style-citations-from-a-bibtex-file-in-emacs
;;http://tex.stackexchange.com/a/31992/5701

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/Dropbox/BIBTEX/REFERENCES.bib"))

;; How to solve @
(eval-after-load 'reftex-vars
  '(progn 
     (setq reftex-cite-format '((?\C-m . "@%l")))))
     
;; I changed the code, before "[@%l]" cite between brackets

;;-----------------------------------------------------------------------------
;; TECLA TAB para 4 espaços Desativei pq inibe o POLYMODE
;; https://ikiwiki.info/tips/Emacs_and_markdown/
;;(autoload 'markdown-mode "markdown-mode")
;;(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

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

;; So that RefTeX finds my bibliography If you want assign a file to BIBTEX
;;(setq reftex-default-bibliography '("/home/rafatieppo/Dropbox/PROFISSIONAL/DOUTORADO/TESE/PAPER_TESE/PAPER_TESE.bib"))

;; Esse deu erro: TESTANDO: FUNCIONOU PERFEITO
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;;atalhos UTEIS
;;;;;automatic formatting of a section: C-c C-q C-s;
;;;;;section preview: C-c C-p C-s; (see the screenshot on the right)


;; TESTE: USEI ESSE PQ O ACIMA ESTAVA com erro (digitei errado). TESTANDO O SUPERIOR AGORA
;; esse FICA NA RESERVA
;; Usei:
;;(add-hook 'LaTeX-mode-hook (lambda () (turn-on-reftex) (setq reftex-plug-into-AUCTeX t)))
;; fonte:http://stackoverflow.com/questions/5722816/reftex-in-emacs-menu-bar
;;-----------------------------------------------------------------------------

;;===========================================================================
;;AUTO COMLETE
;;===========================================================================
(setq ac-math-unicode-in-math-p t)

```lisp
(defvar ac-source-math-latex-everywhere;;
'((candidates . ac-math-symbols-latex)
  (prefix . ac-math-prefix)
  (action . ac-math-action-latex)
  (symbol . "l")))
```
(require 'ac-math) ; This is not needed when you install from MELPA
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources)))

;;USAR ESSE SOMENTE SE NAO USAR O AUCTEX
;;(add-hook 'TeX-mode-hook 'ac-latex-mode-setup) 

(ac-flyspell-workaround)

;;PARA USAR COM O AUCTEX
;;Put this file into your load-path and add the following into your init file.

   (require 'auto-complete-latex)

;; If necessary, add the following into your init file.
   (setq ac-modes (append ac-modes '(foo-mode)))
   (add-hook 'foo-mode-hook 'ac-l-setup)



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

;(add-to-list 'load-path "/home/rafatieppo/.emacs.d/emacs-color-theme-solarized-master")
;(require 'solarized-dark-theme)
;(require 'solarized-definitions)
;;(require 'solarized-theme)
;(require 'color-theme-solarized)

;(custom-set-variables '(solarized-termcolors 256))
;(custom-set-variables '(solarized-contrast 'high))
;(custom-set-variables '(solarized-visibility 'high))

(add-to-list 'load-path "/home/rafatieppo/.emacs.d/themess")

(require 'monokai-theme)
;;(require 'Amelie-theme)
;;(require 'assemblage-theme)
;;(require 'soothe-theme)
;;(require 'deep-thought-theme)
;;(require 'erosiond-theme)
;;(require 'zenburn-theme)
;;(require 'hickey-theme)
;;(require 'spolsky-theme)
;;(require 'fogus-theme)
;;(require 'junio-theme)
;;(require 'wilson-theme)
;;(require 'granger-theme)
;;(require 'odersky-theme)
;;(require 'tangotango-theme)
(require 'atom-one-dark-theme)
;(require 'seti-theme)
;;(require 'ample-zen-theme)
;;(require 'underwater-theme)
;;(require 'moe-dark-theme)
;;(require 'molokai-theme)
;;(require 'dracula-themes)

;;===========================================================================
;;TEMA VEM PADRAO EMACS
;;===========================================================================

;;---------------------------------------------------------------------------
;; COR DE FUNDO cursor Macro highlight-current-line.el
;; http://www.emacswiki.org/emacs/highlight-current-line.el
;;(require 'highlight-current-line)
;;(highlight-current-line-on t)
;; To customize the background color
;; Para ver a lista de cores 
;; M-x list-color-display
;;(set-face-background 'highlight-current-line-face "#1F0F0F") ;; soothe
;;(set-face-background 'highlight-current-line-face "#2F2F2F") ;;MONOKAI
;;(set-face-background 'highlight-current-line-face   "#121212") ;; HICKEY#ffff0
;;(set-face-background 'highlight-current-line-face   "#ffff00") ;; gold

;;Padrao do EMACS cursor linha; Must be after of THEME to do not overlayer
;;http://stackoverflow.com/questions/2718189/emacshighlight-the-current-line-by-underline-it/2718543#2718543
(global-hl-line-mode 1)
;; Underline in current line
(set-face-attribute hl-line-face nil :underline t)
;;(set-face-background hl-line-face "#2F2F2F") ;;MONOKAI
;(set-face-background hl-line-face "#191970") ;; midnightblue
;;(set-face-background hl-line-face "#1a1a1a") ;; darkgray  
;;(set-face-background hl-line-face "#1F0F0F") ;; soothe  
;;(set-face-background hl-line-face "#8b1a1a") ;; firebrick4  
;;(set-face-background hl-line-face "#68228b") ;; darkorchid4
;;(set-face-background hl-line-face "#008b8b") ;; darkcyan 
;;(set-face-background hl-line-face "#0a0a0a") ;; gray4
;;(set-face-background hl-line-face "#121212") ;; gray7
;;---------------------------------------------------------------------------
;;===========================================================================


;;===========================================================================
;; CONFIGURACOES AVANCADAS AUCTEX
;;===========================================================================
;;http://tex.stackexchange.com/questions/161797/how-to-configure-emacs-and-auctex-to-perform-forward-and-inverse-search

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(show-paren-mode t))
 ;; '(TeX-source-correlate-method (quote synctex))
 ;; '(TeX-source-correlate-mode t)
 ;; '(TeX-source-correlate-start-server t)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; MINIMAP (desativei, conflito com o clone)
;;===========================================================================
;;-----------------------------------------------------------------------------
;;(require 'minimap)
;;-----------------------------------------------------------------------------

;;===========================================================================
;; VERTICAL INDENTATION GUIDES
;;===========================================================================
;;http://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs
;;https://github.com/antonj/Highlight-Indentation-for-Emacs/blob/
;;master/highlight-indentation.el
;; other is better
;;(require 'highlight-indentation)
;;-----------------------------------------------------------------------------


;;===========================================================================
;; INDENT GUIDE
;;===========================================================================
;; https://raw.githubusercontent.com/zk-phi/indent-guide/master/indent-guide.el
(require 'indent-guide)
(setq indent-guide-recursive t)
;;-----------------------------------------------------------------------------

;;===========================================================================

;;===========================================================================
;; FOLDING BY INDENTATION
;;===========================================================================
;; Folding code blocks based on indentation.
;; git clone https://github.com/zenozeng/yafolding.el.git

(require 'yafolding)

(global-set-key [?\C-{] #'yafolding-hide-parent-element)
(global-set-key [?\C-}] #'yafolding-toggle-element)
;;-----------------------------------------------------------------------------


;;===========================================================================
;; ADD HIGHLIGHT FOR CERTAIN KEYWORDS
;;===========================================================================
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
 



