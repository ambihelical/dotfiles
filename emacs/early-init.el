;;; -*- lexical-binding: t; -*-

;; disable file-handlers during startup for another 10% saving
(defvar me:file-name-handler-alist file-name-handler-alist)

;; define these early since we need to know the cache directory below
(defconst me:cache-directory (or (getenv "XDG_CACHE_HOME") (expand-file-name ".cache" "~")))
(defconst me:emacs-cache-dir (expand-file-name "emacs" me:cache-directory))

;; redirect eln-cache (based on no-littering recommendation for emacs 29 or later)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" me:emacs-cache-dir))))

;; disable GC at startup, this is fixed later by gcmh
(setq gc-cons-threshold most-positive-fixnum    ; disable gc, fixed by gcmh
      package-quickstart t                      ; 10% faster, I'll take it
      file-name-handler-alist nil               ; faster startup without this
      frame-resize-pixelwise t                  ; disable rounding of frame size
      no-littering-var-directory (expand-file-name "var/" me:emacs-cache-dir)
      package-quickstart-file (expand-file-name "var/package-quickstart.el" me:emacs-cache-dir)
      package-user-dir (expand-file-name "elpa/" me:emacs-cache-dir)
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

