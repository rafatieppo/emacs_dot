;;; install_package.el --- for install packs

;; Copyright (C) 2021 Rafael Tieppo

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
;;  ATTENTION: Run it in terminal in batch mode.
;;  emacs --script install_packages.el

;; ------------------------------------------------------------
;;; Code:

(require 'package)
(package-initialize)

;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;(add-to-list 'package-archives
;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;(add-to-list 'package-archives
;             '("elpa" . "http://tromey.com/elpa/") t)

;(add-to-list 'package-archives
;             '("gnu" . "http://elpa.gnu.org/packages/") t)

;(add-to-list 'package-archives
;            '("melpa" . "https://melpa.org/packages/") t)

;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; ATTENTION: Run the instructions bellow to update packages.
(package-list-packages)    ;; Show packages.
(package-refresh-contents) ;; Run `M-x package-refresh-contents'.
                           ;; Type `U' to mark to upgrade.
                           ;; Press `X' to execute.

;;----------------------------------------------------------------------
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

; Runs the installation of each package.

(package-install 'auctex)
(package-install 'autothemer)
(package-install 'company)
;(package-install 'eglot)
(package-install 'ess)
;(package-install 'ess-view)
;(package-install 'ess-R-data-view)
(package-install 'evil)
;(package-install 'evil-collection)
;(package-install 'flycheck)
;(package-install 'flymake)
(package-install 'helm)
;(package-install 'indent-guide)
(package-install 'lsp-mode)
(package-install 'lsp-ui)
(package-install 'markdown-mode)
(package-install 'magit)
(package-install 'multiple-cursors)
(package-install 'neotree)
(package-install 'org-bullets)
(package-install 'org-tree-slide)
(package-install 'php-mode)
(package-install 'powerline)
(package-install 'projectile)
(package-install 'py-autopep8)
;;(package-install 'lsp-python-ms)
(package-install 'smartparens)
(package-install 'yafolding)
(package-install 'yasnippet)
(package-install 'web-mode)

(provide 'install_package)
;;; install_packages ends here

  
