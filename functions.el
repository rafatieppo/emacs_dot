;;-----------------------------------------------------------------------------
;; Funções inserir o dia e a hora no buffer.
;; http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defun today ()
"Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
(interactive) ; permit invocation in minibuffer
;; (insert (format-time-string "%A, %e de %B , %Y"))
(insert (format-time-string "%e/%m/%Y"))
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
(defun header ()
"Insere cabeçalho."
(interactive)
;; (insert (comment-dwim 2) (make-string 65 ?=) "\n")
(insert (make-string 65 ?=) "\n")
(insert (make-string 45 ? ) "Rafael Tieppo\n")
(insert (make-string 45 ? ) "tiepporc@unemat.br\n")
(insert (make-string 45 ? ) (format-time-string "%d-%m-%Y\n"))
(insert (make-string 65 ?=) "\n")
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
(defun header_org ()
"Insere cabecalho org-mode"
(interactive)
(insert (make-string 0 ? ) "#+LaTeX_CLASS_OPTIONS: [article,a4paper,times,12pt] \n")
(insert (make-string 0 ? ) "#+latex_header: \\usepackage[hmargin=2.5cm,vmargin=2.5cm]{geometry} \n")
(insert (make-string 0 ? ) "#+LaTeX_HEADER: \\usepackage[alf,abnt-repeated-title-omit=yes,abnt-show-options=warn,abnt-emphasize=bf,abnt-etal-list=0]{abntcite} \n") 
(insert (make-string 0 ? ) "#+LaTeX_HEADER: \\usemintedstyle{tango} \n")
(insert (make-string 0 ? ) "#+TITLE: Title \n")
(insert (make-string 0 ? ) "#+DATE: \n")
(insert (make-string 0 ? ) "#+AUTHOR: Rafael Tieppo \n")
(insert (make-string 0 ? ) "#+EMAIL: tiepporc@unemat.br \n")
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
(defun header_md ()
"Insere cabecalho markdown-mode"
(interactive)
(insert (make-string 0 ? ) "--- \n")
(insert (make-string 0 ? ) "title: TITLE \n")
(insert (make-string 0 ? ) "author: Rafael Tieppo \n")
(insert (make-string 0 ? ) "date: Maio, 201 \n")
(insert (make-string 0 ? ) "--- \n")
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
(defun figure_md ()
"Insere cabecalho markdown-mode"
(interactive)
(insert (make-string 0 ? ) "![caption here](file here.png){#fig:label_here} \n")
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Insert a new (empty) chunk to R markdown.
(defun insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```R\n\n```")
  (forward-line -1)
  )

(global-set-key (kbd "C-c i") 'insert-chunk)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Função para duplicar linhas (esse comando/atalho é muito útil).
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;; Define função para duplicar linhas.
(defun duplicate-line ()
(interactive)
(move-beginning-of-line 1)
(kill-line)
(yank)
(newline)
(yank)
)
;; Define atalho para duplicar linhas.
(global-set-key (kbd "\C-c d") 'duplicate-line)
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el
;; Salva lista de aquivos recentes. Abrir lista com C-x f
(recentf-mode 1)
(setq recentf-max-saved-items 25) ;; 20 é muito pouco.
;; Salva histórico de comandos do minibuffer.
(savehist-mode 1)
(setq history-length 1000)
;;---------------------------------------------------------------------------



