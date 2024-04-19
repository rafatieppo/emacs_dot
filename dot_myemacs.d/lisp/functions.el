;;------------------------------------------------------------
;; Insert date Funções inserir o dia e a hora no buffer.
(defun today ()
  "Insert string for today's date nicely formatted in American style, e.g. Sunday, September 17, 2000."
  (interactive) ; permit invocation in minibuffer
  ;; (insert (format-time-string "%A, %e de %B , %Y"))
  (insert (format-time-string "%Y/%m/%d"))
  )

;;------------------------------------------------------------
;; Insert headerS
(defun header ()
  "Insere cabeçalho."
  (interactive)
  ;; (insert (comment-dwim 2) (make-string 65 ?=) "\n")
  (insert "#" (make-string 70 ?=) "\n")
  (insert "#" (make-string 40 ? ) "Rafael Tieppo\n")
  (insert "#" (make-string 40 ? ) "rafaeltieppo@yahoo.com.br\n")
  (insert "#" (make-string 40 ? ) "https://rafatieppo.github.io/\n")
  (insert "#" (make-string 40 ? ) (format-time-string "%Y-%m-%d\n"))
  (insert "#" (make-string 70 ?=) "\n")
  )

;;------------------------------------------------------------
(defun header_md ()
  "Insere cabecalho markdown-mode"
  (interactive)
  (insert (make-string 0 ? ) "--- \n")
  (insert (make-string 0 ? ) "title: TITLE \n")
  (insert (make-string 0 ? ) "author: Rafael Tieppo \n")
  (insert (make-string 0 ? ) "date: Maio, 201 \n")
  (insert (make-string 0 ? ) "--- \n")
  )

;;------------------------------------------------------------
(defun figure_md ()
  "Insere cabecalho markdown-mode"
  (interactive)
  (insert (make-string 0 ? ) "![cap](PICS/file.png){width=16cm height=9cm #fig:figbal} \n")
  )

;;------------------------------------------------------------
(defun header_org_beamer()
  "Insert header for export beamer-pdf"
  (interactive)

(insert (make-string 0 ? ) "#+title: Disciplina: Estatística\n")
(insert (make-string 0 ? ) "#+subtitle: Análise exploratória\n")
(insert (make-string 0 ? ) "#+date: 2023-12-23\n")
(insert (make-string 0 ? ) "#+author: Rafael Tieppo\n")
(insert (make-string 0 ? ) "#+email: rafatieppo@rt-avl52\n")
(insert (make-string 0 ? ) "#+OPTIONS: H:3\n")
(insert (make-string 0 ? ) "#+BEAMER_EXPORT: \\usepackage{multicol}\n")
(insert (make-string 0 ? ) "#+BEAMER_EXPORT: \\usepackage{listings}\n")
(insert (make-string 0 ? ) "#+BEAMER_EXPORT: \\usepackage{minted}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setminted{style=solarized-light,frame=leftline}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setminted[r]{linenos=true}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\usepackage[utf8]{inputenc}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\usepackage[brazil]{babel}\n")
(insert (make-string 0 ? ) "#+BEAMER_THEME: metropolis\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\defbeamertemplate*{footline}{mytheme} {\\begin{beamercolorbox}[wd=.5\\paperwidth,ht=0.5ex,dp=0.025ex,leftskip=.1cm,rightskip=.3cm]{title in head/foot} \\makebox[2em][l]{{\\usebeamerfont{title in head/foot}\\textcolor{black}{\\insertframenumber}}} \\end{beamercolorbox}   \\begin{beamercolorbox}[wd=.2\\paperwidth,ht=0.5ex,dp=1.125ex,leftskip=.6cm,rightskip=.3cm]{date in head/foot} \\usebeamerfont{}\\textcolor{black}{Rafael Tieppo} \\end{beamercolorbox}}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\logo{\\includegraphics[height=.4cm]{/home/rafatieppo/Dropbox/logo/logo_ppgasp.png}\\vspace{-0.5cm}\\hspace{0.5cm}}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\useoutertheme[subsection=true]{tree} %smoothbars tree\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\definecolor{UBCblue}{rgb}{0.04706, 0.13725, 0.26667} % UBC Blue  (primary)\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\definecolor{RTblue}{rgb}{0.25, 0.22 , 0.38} % UBC Blue 0.04706, 0.13725, 0.26667 (primary)\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\definecolor{RTtop}{rgb}{0, 63 , 92}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\definecolor{RTmain}{RGB}{64, 64, 64}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\definecolor{RTback}{RGB}{239, 239, 239}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\definecolor{RTblueti}{RGB}{2, 119, 187}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{title in head/foot}{bg=,fg=RTblue}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{section in head/foot}{bg=RTmain,fg=white} %bg=UBCblue,fg=white\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{subsection in head/foot}{bg=RTmain,fg=white} %bg=UBCblue,fg=white %gray!10\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{subsubsection in head/foot}{bg=,fg=red}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamertemplate{background canvas}[vertical shading][bottom=gray!20, middle=gray!20, top=gray!50]\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{frametitle}{bg=,fg=RTblueti} % slite level 3 title\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{block title}{bg=UBCgrey,fg=white}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{block body}{bg=gray!10}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamertemplate{blocks}[rounded][shadow=true]\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{itemize item}{fg=orange}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{itemize subitem}{fg=cyan}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamercolor{itemize subsubitem}{fg=cyan}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamertemplate{itemize item}[circle]\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamertemplate{itemize subitem}[triangle]\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamertemplate{itemize subsubitem}[triangle]\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\setbeamertemplate{itemize subsubitem}[triangle]\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\usepackage{caption}\n")
(insert (make-string 0 ? ) "#+BEAMER_HEADER: \\captionsetup{font=scriptsize,labelfont=footnotesize}\n")
(insert (make-string 0 ? ) "#+BIBLIOGRAPHY: /home/rafatieppo/Dropbox/bibtex/references.bib\n")
(insert (make-string 0 ? ) "#+CITE_EXPORT: csl /home/rafatieppo/Dropbox/csl/abnt-ipea.csl\n")
  )


;;------------------------------------------------------------
(defun header_org_latex()
  "Insert header dor export Latex export"
  (interactive)
  (insert (make-string 0 ? ) "#+title: Estatística\n")
    (insert (make-string 0 ? ) "#+subtitle: Análise exploratória\n")
    (insert (make-string 0 ? ) "#+date: 2023-12-23\n")
    (insert (make-string 0 ? ) "#+author: Rafael Tieppo\n")
    (insert (make-string 0 ? ) "#+email: rafatieppo@rt-avl52\n")
    (insert (make-string 0 ? ) "#+OPTIONS: H:3 toc:nil ^:nil ^:{}\n")
    (insert (make-string 0 ? ) "#+SETUPFILE: /home/rafatieppo/Dropbox/sty/org_standard.org\n")
    (insert (make-string 0 ? ) "#+BIBLIOGRAPHY: /home/rafatieppo/Dropbox/bibtex/references.bib\n")
    (insert (make-string 0 ? ) "#+CITE_EXPORT: csl /home/rafatieppo/Dropbox/csl/abnt-ipea.csl\n")
    )
;;------------------------------------------------------------
;; Insert a new (empty) chunk to R markdown. Changed to ```{r}```, before was ```R\n\n```
(defun insert-chunk ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r}\n\n```")
  (forward-line -1)
  )
(global-set-key (kbd "C-c i") 'insert-chunk)

;;------------------------------------------------------------
;; Insert a new (empty) FULL chunk to R markdown.
(defun insert-chunk-full ()
  "Insert chunk environment Rmd sessions."
  (interactive)
  (insert "```{r, echo = TRUE, results = 'markup', eval = TRUE}\n\n```")
  (forward-line -1)
  )
(global-set-key (kbd "C-c r") 'insert-chunk-full)

;;;------------------------------------------------------------
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
;; (un)Comment line or region
(global-set-key (kbd "M-;") 'comment-line-or-region)

;;;------------------------------------------------------------
;;; Salva lista de aquivos recentes. Abrir lista com C-x f https://github.com/magnars/
(recentf-mode 1)
(setq recentf-max-saved-items 25) ;;
(savehist-mode 1)
(setq history-length 500)

;;------------------------------------------------------------
;; Text manipulation
;;------------------------------------------------------------

;; Função para duplicar linhas
(defun duplicate-line ()
  "Duplicate line ..."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  )
;; Define atalho para duplicar linhas.
(global-set-key (kbd "\C-c d") 'duplicate-line)

;;------------------------------------------------------------
;; Move lines. ;; http://www.emacswiki.org/emacs/MoveLine
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
    (forward-char col))
  )
(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n)))
  )
(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n))
  )
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; fill-paragraph. Takes a multi-line paragraph and makes ;;; it into a single line of text.
;; https://josephhall.org/nqb2/index.php/unfill
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil))
  )

;; add a string at enf of line several times - from myself
(defun add-str-end (tex_end n)
  "prompt for string, add it to end of lines in the region"
  (interactive "sEnter the string to add at end of line:
nEnter how many times: ")
  (let ((x 0))
    (while (< x n)
      (end-of-line)
      (insert (make-string 0 ? ) tex_end)      
      (next-line)
      (setq x (1+ x))
      )))

;;------------------------------------------------------------
;; ORG-MODE
;;------------------------------------------------------------

;; Return schedule duration
(defun schedule-duration-minutes (schedule-stamp)
  "Return schedule duration in hours from a schedule stamp"
  (interactive)
  (setq sa (replace-regexp-in-string "\\`<+\\|>" "" schedule-stamp))
  (setq sb (split-string sa " " ))
  (/
   (- (org-duration-to-minutes (nth 1 (split-string (nth 2 sb) "-" )))
      (org-duration-to-minutes (nth 0 (split-string (nth 2 sb)  "-" ))))
   60))

;; ORG MONTH REPORT
(defun my/org-review-month (start-date)
  "Review the months clocked tasks and time from START-DATE ..."
  (interactive (list (org-read-date)))
  ;; Set to the beginning of the month
  (setq start-date (concat (substring start-date 0 8) "01"))
  (let ((org-agenda-show-log t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)))
    (org-agenda-list nil start-date 'month))
  )
  
;; ORG REPORTING TIME BY DAY
;; http://sachachua.com/blog/2007/12/clocking-time-with-emacs-org/
(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports PARAMS ..."
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

;;------------------------------------------------------------
;; PYTHON
;;------------------------------------------------------------

;; ALT ENTER to send line
(defun my-python-line ()
  "Sends only one line from python mode buffer to a python shell ..."
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
    (yank)
    )
  (beginning-of-line) ;; or (end-of-line)
  (next-line)
  )
(global-set-key (kbd "M-RET") 'my-python-line) ; Enter/Return key

;;-----------------------------------------------------------------------------
;; send paragraph to eval in terminal
(defun python-my-python-parag ()
  "Sends a paragraph from python mode buffer to a python shell ..."
  (interactive)
  (save-excursion
    (setq the_script_buffer (format (buffer-name)))
    (backward-paragraph) ;    (end-of-line)
    (kill-region (point) (progn (forward-paragraph) (point)))
    (setq the_py_buffer (format "*Python[%s]*" (buffer-file-name)))
    (setq the_py_buffer "*Python*")
    (switch-to-buffer-other-window  the_py_buffer)
    (goto-char (buffer-end 1))
    (yank)
    (comint-send-input)
    (switch-to-buffer-other-window the_script_buffer)
    (yank)
    )
  (beginning-of-line) ;; or (end-of-line)
  (next-line)
  (forward-paragraph)
  )

(global-set-key (kbd "M-\\") 'python-my-python-parag)

;; ALT / to send region https://emacs.stackexchange.com/questions/27674/make-elpy-shell-send-more-intelligent
(defun python-shell-send-region-or-line nil ()
  "Sends a region from python mode buffer to a python shell, intelligently ..."
  (interactive)
  (cond ((region-active-p)
         (setq deactivate-mark t)
         (python-shell-send-region (region-beginning) (region-end))
         (python-nav-forward-statement)
         ) (t (elpy-shell-send-current-statement))))
(global-set-key (kbd "M-/") 'python-shell-send-region-or-line) ; alt + /

;; ALT p to send paragraph
(defun python-shell-send-parag-step ()
  "Sends the current paragraph to the python REPL and go to the next one ..."
  (interactive)
  (cond (
         (mark-paragraph)
         (python-shell-send-region (region-beginning) (region-end))
         (deactivate-mark)
         (python-nav-forward-statement)
         ) (t (elpy-shell-send-current-statement))))
(global-set-key (kbd "M-p") 'python-shell-send-parag-step) ; alt + p

;;------------------------------------------------------------
;; TECLAS DE ATALHO
;;------------------------------------------------------------

;; Define C-TAB para mudar o cursor de janelas (buffers ativos).
(global-set-key [(control tab)] 'other-window)

;; enable iswitchb mode: C-x b now shows a list of buffers
(global-set-key (kbd "C-<next>") 'next-buffer)

;; Define C-page down e C-page up para mover entre buffers.
(global-set-key (kbd "C-<prior>") 'previous-buffer)

;; Define C-F4 para fechar um buffer.
(define-key global-map [(control f4)] 'kill-buffer)

;; Define C-F1 para (des)ativar o flyspell.
(define-key global-map [(control f1)] 'flyspell-mode)

;; Para passar o corretor ortográfico em uma região.
(define-key global-map [(control f2)] 'flyspell-region)

;; Define C-- para fazer linha com 60 sinais de -.
(global-set-key [?\C--] (kbd "C-u 6 0 -"))

;; Define F11 e F12 para trocar entre modo com e sem menus.
;; (global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f10>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<f11>") 'toggle-tool-bar-mode-from-frame)

;; multiple-cursors.el ;; continuous lines
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-. C-c") 'mc/edit-lines)
;; not based on continuous lines
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-; C-<") 'mc/mark-all-like-this)

;;-----------------------------------------------------------------------------
;; FUNCTION HIGHLIGHTS LISP
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;;-----------------------------------------------------------------------------

;;;(provide 'functions)
;;; functions.el ends here
