;;; -*- lexical-binding: t -*-

;; package management

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package )
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package eglot 
  :ensure t
  :hook ((c++-mode . eglot-ensure)))

(use-package evil
  :ensure t
  :hook (( prog-mode text-mode ) . evil-mode ))
