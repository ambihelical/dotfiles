;;; -*- lexical-binding: t; -*-

;; disable GC at startup, this is fixed later by gcmh
(setq gc-cons-threshold most-positive-fixnum)

;; use quickstart feature for 10% faster startup
(setq package-quickstart t)

;; ?
;;(setq package-enable-at-startup nil)

;; disable file-handlers during startup for another 10% saving
(defvar me:file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me:file-name-handler-alist)
            (makunbound 'me:file-name-handler-alist)))
