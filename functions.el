;;-----------------------------------------------------------------------------
;; Insert date
;;-----------------------------------------------------------------------------

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
;; Insert headerS
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
(defun header ()
"Insere cabeçalho."
(interactive)
;; (insert (comment-dwim 2) (make-string 65 ?=) "\n")
(insert (make-string 65 ?=) "\n")
(insert (make-string 45 ? ) "Rafael Tieppo\n")
(insert (make-string 45 ? ) "rafaelt@unemat.br\n")
(insert (make-string 45 ? ) "http://docente.unemat.br/rafaeltieppo/\n")
(insert (make-string 45 ? ) (format-time-string "%d-%m-%Y\n"))
(insert (make-string 65 ?=) "\n")
)

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
(insert (make-string 0 ? ) "![cap](file){#fig:label} \n")
)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Insert a new (empty) chunk to R markdown.
;; Changed to ```{r}```, before was ```R\n\n```
;;-----------------------------------------------------------------------------
(defun insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r}\n\n```")
  (forward-line -1)
  )

(global-set-key (kbd "C-c i") 'insert-chunk)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Insert a new (empty) FULL chunk to R markdown.
;;-----------------------------------------------------------------------------
(defun insert-chunk-full ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r, echo = TRUE, result = 'hide', eval = TRUE}\n\n```")
  (forward-line -1)
  )

(global-set-key (kbd "C-c r") 'insert-chunk-full)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Função para duplicar linhas
;;-----------------------------------------------------------------------------
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
;;-----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-saved-items 25) ;; 20 é muito pouco.
;; Salva histórico de comandos do minibuffer.
(savehist-mode 1)
(setq history-length 500)
;;---------------------------------------------------------------------------



