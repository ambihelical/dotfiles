;;; -*- lexical-binding: t; -*-

;; disable file-handlers during startup for another 10% saving
(defvar me:file-name-handler-alist file-name-handler-alist)

;; redirect eln-cache (per no-littering recommendation for emacs 29 or later)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; disable GC at startup, this is fixed later by gcmh
(setq gc-cons-threshold most-positive-fixnum    ; disable gc, fixed by gcmh
      package-quickstart t                      ; 10% faster, I'll take it
      file-name-handler-alist nil               ; faster startup without this
      frame-resize-pixelwise t                  ; disable rounding of frame size
      package--init-file-ensured t              ;
      package-enable-at-startup nil)            ; don't make packages available at start

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me:file-name-handler-alist)
            (makunbound 'me:file-name-handler-alist)))

;; package management
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

