;;; -*- lexical-binding: t; -*-

;; disable file-handlers during startup for another 10% saving
(defvar me:file-name-handler-alist file-name-handler-alist)

;; disable GC at startup, this is fixed later by gcmh
(setq gc-cons-threshold most-positive-fixnum    ; disable gc, fixed by gcmh
      package-quickstart t                      ; 10% faster, I'll take it
      file-name-handler-alist nil               ; faster startup without this
      package-enable-at-startup nil)            ; ?

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me:file-name-handler-alist)
            (makunbound 'me:file-name-handler-alist)))

