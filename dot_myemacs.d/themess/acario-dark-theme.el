;;; acario-dark-theme.el --- Dark variant of Acario.
;; Copyright (C) 2019 Gerry Agbobada

;; Author: Gerry Agbobada
;; URL: https://framagit.org/gagbo/acario-theme

;; Version: 0.1.0
;; Keywords: faces
;; Package-Requires: ((emacs "24"))

;; This file is not part of Emacs.

;;; Commentary:
;; Dark variant of Acario

;;; Code:
(require 'acario-themes)

(deftheme acario-dark "Acario dark variant.")

(create-acario-theme 'dark 'acario-dark)

(provide-theme 'acario-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; acario-dark-theme.el ends here

