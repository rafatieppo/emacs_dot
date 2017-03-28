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
  (insert "```{r, echo = TRUE, results = 'markup', eval = TRUE}\n\n```")
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
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Move lines. ;; http://www.emacswiki.org/emacs/MoveLine
;;-----------------------------------------------------------------------------
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
(move-line (if (null n) 1 n)))
;;-----------------------------------------------------------------------------

(defun comment-line-or-region ()
  "Comment or uncomment current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region
       (region-beginning)
       (region-end)
       )
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-beginning-position 2)
     )
    )
  )
;;-----------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; Salva lista de aquivos recentes. Abrir lista com C-x f
;; https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el
;;-----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-saved-items 25) ;; 20 é muito pouco.
;; Salva histórico de comandos do minibuffer.
(savehist-mode 1)
(setq history-length 500)
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------
;; fill-paragraph. Takes a multi-line paragraph and makes ;;; it into a single line of text.
;; https://josephhall.org/nqb2/index.php/unfill
;;-----------------------------------------------------------------------------
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
;;-----------------------------------------------------------------------------





;;===========================================================================
;; TECLAS DE ATALHO
;;===========================================================================
;;----------------------------------------------------------------------------- 
;; Define C-TAB para mudar o cursor de janelas (buffers ativos).
(global-set-key [(control tab)] 'other-window)
;;-----------------------------------------------------------------------------
;; enable iswitchb mode: C-x b now shows a list of buffers
;; ref: http://emacs-fu.blogspot.com.br/2009/02/switching-buffers.html
;; Define C-page down e C-page up para mover entre buffers.
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
;;-----------------------------------------------------------------------------
;; Define C-F4 para fechar um buffer.
(define-key global-map [(control f4)] 'kill-buffer)
;;-----------------------------------------------------------------------------
;; Define C-F1 para (des)ativar o flyspell.
(define-key global-map [(control f1)] 'flyspell-mode)
;;-----------------------------------------------------------------------------
;; Para passar o corretor ortográfico em uma região.
(define-key global-map [(control f2)] 'flyspell-region)
;;-----------------------------------------------------------------------------
;; Define C-- para fazer linha com 60 sinais de -.
(global-set-key [?\C--] (kbd "C-u 6 0 -"))
;;-----------------------------------------------------------------------------
;; Define F11 e F12 para trocar entre modo com e sem menus.
;; (global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f10>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<f11>") 'toggle-tool-bar-mode-from-frame)
;;-----------------------------------------------------------------------------
;; multiple-cursors.el ;; continuous lines
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-. C-c") 'mc/edit-lines)
;; not based on continuous lines
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-; C-<") 'mc/mark-all-like-this)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;-----------------------------------------------------------------------------
;; FUNCTION HIGHLIGHTS LISP
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;;-----------------------------------------------------------------------------
;; Define atalho para duplicar linhas.
(global-set-key (kbd "\C-c d") 'duplicate-line)
;;-----------------------------------------------------------------------------
;; Move lines.
(global-set-key (kbd "M-<") 'move-line-up)
(global-set-key (kbd "M->") 'move-line-down)

;;-----------------------------------------------------------------------------
;; (un)Comment line oir region
(global-set-key (kbd "M-;") 'comment-line-or-region)



