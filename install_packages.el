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

;; best
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;(add-to-list 'package-archives
;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;(add-to-list 'package-archives
;             '("elpa" . "http://tromey.com/elpa/") t)

;(add-to-list 'package-archives
;             '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/") t)

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

(package-install 'ace-jump-mode)
;(package-install 'ac-math)
(package-install 'all-the-icons)
;(package-install 'anaconda-mode)
(package-install 'auctex)
(package-install 'auto-complete)
(package-install 'citeproc)
;(package-install 'calfw)
;(package-install 'calfw-org)
;(package-install 'calfw-ical)
;(package-install 'company-irony)
(package-install 'company-mode)
(package-install 'company-auctex)
;(package-install 'company-jedi)
;(package-install 'company-quickhelp)
;(package-install 'company-tern)
;(package-install 'company-c-headers)
;(package-install 'company-clang)
;(package-install 'company-irony-c-headers)
;(package-install 'doom-modeline)
;(package-install 'elpy) ;;install via apt - check shell scrit
(package-install 'ess)
;(package-install 'ess-view)
;(package-install 'ess-R-data-view)
(package-install 'evil)
;(package-install 'flx)
;(package-install 'flx-ido)
;(package-install 'flycheck)
;(package-install 'flymake)
(package-install 'helm)
(package-install 'highlight-symbol)
;(package-install 'ido)
;(package-install 'ido-hacks)
;(package-install 'ido-vertical-mode)
(package-install 'indent-guide)
(package-install 'lsp-mode)
(package-install 'lsp-ui)
;(package-install 'jedi)
(package-install 'markdown-mode)
(package-install 'magit)
(package-install 'org-bullets)
(package-install 'org-tree-slide)
;(package-install 'js2-mode)
(package-install 'multiple-cursors)
(package-install 'neotree)
;(package-install 'polymode)
;(package-install 'poly-markdown)
;(package-install 'poly-R)
;(package-install 'popup)
;(package-install 'powerline)
(package-install 'projectile)
(package-install 'py-autopep8)
;;(package-install 'lsp-python-ms)
(package-install 'rainbow-delimiters)
(package-install 'smartparens)
;(package-install 'smex)
(package-install 'yafolding)
(package-install 'yasnippet)
(package-install 'use-package)
(package-install 'web-mode)

(provide 'install_package)
;;; install_packages ends here

;; NOTE: after install `jedi` and `jedy-core`, run `M-x
;; jedi:install-server` in a new GNU Emacs session to enable proper auto
;; completation for Python scripts.
;; irony-install-server for c and cpp `sudo apt install llvm-X clang-X libclang-X-dev`

