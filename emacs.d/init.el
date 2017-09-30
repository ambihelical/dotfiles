;;; -*- lexical-binding: t -*-

(defconst me:gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold me:gc-cons-threshold-original)))

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
  (setq use-package-enable-imenu-support t)                       ; support for packages in imenu
  (require 'use-package))

(eval-after-load "use-package"
  '(setq use-package-always-ensure t                              ; ensure by default
         use-package-always-defer t                               ; defer by default
         use-package-minimum-reported-time 0.03                   ; minimum time when verbose
         use-package-verbose nil))                                 ; don't be verbose

;; set some personal variables
(defconst me:data-directory (or (getenv "XDG_DATA_HOME") "~/.local/share"))
(defconst me:cache-directory (or (getenv "XDG_CACHE_HOME")  "~/.cache"))
(defconst me:config-directory (or (getenv "XDG_CONFIG_HOME")  "~/.config"))
(defconst me:emacs-backup-directory (expand-file-name "emacs" me:cache-directory))

;; replace prefix part of a string
(defun me:replace-prefix (prefix input)
  (replace-regexp-in-string ( concat "^" (regexp-quote prefix)) "" input))

;; replace any matches in a string
(defun me:replace-all (input from to)
  (replace-regexp-in-string (regexp-quote from) to input nil))

;; run hook function on buffer after a delay
(defmacro me:add-hook-with-delay (hook delay func)
    `(add-hook ,hook
               (lambda ()
                 (let ((cur-buf (current-buffer)))
                   (run-at-time ,delay nil
                                (lambda ()
                                  (save-current-buffer
                                    (if (buffer-live-p cur-buf)
                                        (progn
                                          (set-buffer cur-buf)
                                          (funcall ,func))))))))))

(defun me:set-preferred-font ()
  (let ((fonts (font-family-list)))
    (if (member "Hack" fonts)
        (set-frame-font "Hack-10:autohint=true" t t)
      (if (member "Fantasque Sans Mono" fonts)
          (set-frame-font "Fantasque Sans Mono-12" t t)
        (if (member "DejaVu Sans Mono" fonts)
            (set-frame-font "DejaVu Sans Mono-11" t t))))))

(defun me:extra-setup ()
  (tool-bar-mode 0)                                           ; no tool bar (tool-bar)
  (scroll-bar-mode 0)                                         ; no scroll bar (scroll-bar)
  (mouse-avoidance-mode 'animate)                             ; move mouse pointer out of way (avoid)
  (global-eldoc-mode -1)                                      ; turn off annoying eldoc mode (eldoc)
  (fset 'yes-or-no-p 'y-or-n-p)                               ; change stupid default y/n? y
  (make-directory me:emacs-backup-directory t)                ; make sure backup dir exists
  (electric-indent-mode +1)                                   ; turn on electric mode globally (electric)
  (me:set-preferred-font)                                     ; set the font
  (run-at-time "1 hour" 3600 #'clean-buffer-list))            ; clear out old buffers every hour (midnight)

;; configuration I haven't figured out how to wedge into
;; use-package

(setq-default tab-width 3                                   ; ideal tab width
              indent-tabs-mode t                            ; enable tabs for most files
              indicate-empty-lines t                        ; show empty lines at end of buffer
              fill-column 120)                              ; auto-wrap only very long lines
(setq auto-save-file-name-transforms
         `((".*" ,me:emacs-backup-directory t))             ; autosave files in backup directory
      ad-redefinition-action 'accept                        ; turn off 'xyz' got redefined warnings
      backup-directory-alist
         `((".*" . ,me:emacs-backup-directory))             ; backup files in backup directory
      custom-file "/dev/null"                               ; disable customizations
      fast-but-imprecise-scrolling t                        ; quick and dirty scrolling
      history-length 1000                                   ; length of history
      inhibit-splash-screen t                               ; no splash
      inhibit-startup-echo-area-message t                   ; no startup message
      inhibit-startup-message t                             ; no startup message
      initial-major-mode 'text-mode                         ; no prog-mode at startup
      initial-scratch-message nil                           ; no scratch message
      load-prefer-newer t                                   ; load source if newer than bytecode
      mouse-wheel-scroll-amount '(3 ((shift) . 9))          ; 3 lines, or 9 line when shift held (mwheel)
      mouse-wheel-follow-mouse 't                           ; scroll window under mouse (mwheel)
      mouse-wheel-progressive-speed nil                     ; don't speed up (mwheel)
      undo-limit 1000000                                    ; 1M (default is 80K)
      undo-strong-limit 1500000                             ; 1.5M (default is 120K)
      undo-outer-limit 150000000                            ; 150M (default is 12M)
      scroll-margin 5                                       ; show some lines around cursor when possible
      safe-local-variable-values                            ; allow these values in .dir-locals.el
        '((evil-indent-convert-tabs . t))
      select-enable-clipboard nil                           ; make cut/paste function correctly (select)
      sentence-end-double-space nil                         ; sentences end with one space
      standard-indent 3                                     ; ideal indent :)
      view-read-only t                                      ; show r/o files in view mode
      x-gtk-use-system-tooltips nil)                        ; allow tooltip theming

(add-hook 'after-init-hook                                  ; report init time
          (lambda ()
            (message "Time to initialize: %s"
                     (emacs-init-time))))
(add-hook 'prog-mode-hook                                   ; stuff for all programming modes
          (lambda ()
             (modify-syntax-entry ?_ "w")))                 ; underscores are parts of words

(add-hook 'makefile-mode-hook
          (lambda ()
            (modify-syntax-entry ?+ "." )))                  ; + is punctuation

(run-with-idle-timer 0.1 nil #'me:extra-setup)


;; keymappings
;; N.B. Other keybindings defined in apropriate use-package
(use-package general
  :init
	(global-unset-key (kbd "<f4>"))
  :config
  (general-evil-setup t)
  (general-define-key
   :prefix "<f4>"
    "g"     #'general-describe-keybindings)
  (general-define-key "s-." #'repeat)
  :demand)

;; frequently used functions
(use-package utilities
  :ensure nil
  :commands (me:set-extra-font-attributes
             me:minibuffer-setup
             me:minibuffer-exit
             me:save-dirty-buffers )
  :general
    ("s-2"        #'me:select-2nd-other-buffer)
    ("s-3"        #'me:select-3rd-other-buffer)
    ("s-4"        #'me:select-4th-other-buffer)
    ("s-5"        #'me:select-5th-other-buffer)
    ("s-v"        #'me:paste-then-earlier)
    ("s-V"        #'me:paste-then-later)
    ("<f3>"       #'me:find-some-files)
  :config
  :load-path "lisp/")

;; infrequently used functions
(use-package extras
  :ensure nil
  :general
    ("s-1"     #'me:find-other-file)
    ("s-c"     #'me:rotate-fill-column)
    ("<f4> 1"  #'me:ps-one-per-page)
    ("<f4> 2"  #'me:ps-two-per-page)
    ("s-\\"    #'me:next-powerline-separator)
  :config
  :load-path "lisp/")

;; built-in emacs-lisp-mode package
(use-package emacs-lisp-mode
  :ensure nil
  :general
  (:prefix "C-c"
           "v"          #'pp-eval-last-sexp
           "x"          #'pp-macroexpand-last-sexp)
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")                    ; hyphens are parts of words
              (setq tab-width 2                               ; tab inserts 2 spaces
                    standard-indent 2                         ; indent by 2
                    indent-tabs-mode nil                      ; no tabs
                    evil-shift-width 2                        ; need this since no tabs
                    lisp-body-indent 2)))                     ; indent elisp by 2
  :interpreter (("emacs" . emacs-lisp-mode)))

;; built-in frame package
;; Because there is no window package, window config is here as well
(use-package frame
  :ensure nil
  :general
  ("<f5> f"     #'toggle-frame-fullscreen)   ; frame
  ("s-`"        #'previous-buffer)           ; window
  ("<s-tab>"    #'next-buffer)               ; window
  ("s-w"        #'other-window)              ; window
  :init
  (add-hook 'focus-out-hook #'me:save-dirty-buffers)          ; save on defocus
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                    (me:replace-prefix (abbreviate-file-name default-directory)
                                        (abbreviate-file-name buffer-file-name))
                  "%b"))
          " %* ["
          (:eval (abbreviate-file-name default-directory))
          "]")                                               ; fancy title
      split-width-threshold 240                              ; 2x ideal line width :)
      icon-title-format frame-title-format)                  ; use same title for unselected frame
  :config
  :demand)

;; built-in winner package
;; N.B. winner-mode must be started early to record window configs
(use-package winner
  :ensure nil
  :general
    ("s-]"   #'winner-redo)
    ("s-["   #'winner-undo)
  :defer 1
  :init
  :config
  (winner-mode t))

;; built-in windmove
(use-package windmove
  :ensure nil
  :defer 3
  :general
  ("s-j" #'windmove-down)
  ("s-k" #'windmove-up)
  ("s-h" #'windmove-left)
  ("s-l" #'windmove-right))


;; built-in minibuffer package
(use-package minibuffer
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)                        ; allow recursive edit
  (add-hook 'minibuffer-setup-hook #'me:minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'me:minibuffer-exit)

  :config
  (savehist-mode t)                        ; save minibuffer history (savehist)
  (minibuffer-depth-indicate-mode t)       ; show recursive edit depth (mb-depth)
  :demand)

;; built-in autorevert package
(use-package autorevert
  :ensure nil
  :defer 2
  :init
  (add-hook 'prog-mode-hook #'auto-revert-mode)
  (add-hook 'text-mode-hook #'auto-revert-mode)
  :config
  (setq auto-revert-check-vc-info nil                         ; don't update branch on auto-revert
        auto-revert-verbose nil)                              ; don't tell me about auto reverts
  :diminish auto-revert-mode)

;; built-in "simple" package
(use-package simple
  :ensure nil
  :general
    ("s-n"        #'next-error)
    ("s-p"        #'previous-error)
  :init
  (add-hook 'prog-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'visual-line-mode-hook (lambda () (diminish 'visual-line-mode "ⓥ")))
  (add-hook 'auto-fill-mode-hook (lambda () (message "here") (diminish 'auto-fill-function "⮨")))
  (setq kill-ring-max 200                      ; More killed items
        kill-do-not-save-duplicates t          ; No duplicates in kill ring
        save-interprogram-paste-before-kill t  ; save clipboard before killing
        visual-line-fringe-indicators
           '(left-curly-arrow nil))            ; use left curly error for wrapped lines
  :config
  (column-number-mode t)                       ; display column/row of cursor in mode-line
  :demand)

;; built-in "abbrev" package
(use-package abbrev
  :ensure nil
  :general
  (:prefix "C-x"
           "a"  '(:ignore t :which-key "Abbrev→" ))
  :init
  :config
  :diminish abbrev-mode
  :demand)

;; built-in "hl-line" package
(use-package hl-line
  :ensure nil
  :general
  ("<f5> h" #'global-hl-line-mode)              ; toggle hl-line
  :init
  :config
  (global-hl-line-mode t)                       ; highlight current line (hl-line)
  :defer 1)

;; built-in "menu-bar" package
(use-package menu-bar
  :ensure nil
  :general
    ("<f5> m"      #'menu-bar-mode)
    ("<f5> <f5>"   #'menu-bar-open)
  :init
  :config
  (menu-bar-mode 0)                             ; no menu bar
  :demand)

;; built-in "face-remap" package
(use-package face-remap
  :ensure nil
  :general
  ("<f5> -" #'text-scale-adjust)
  ("<f5> =" #'text-scale-adjust)
  :init
  (setq text-scale-mode-step 1.05)              ; text size increases by 5% (normally 20%)
  :config
  :defer 3)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t)
  :defer 2
  :init
  (setq smooth-scroll-margin 5
        smooth-scroll-strict-margins t))

(use-package sublimity
  :if window-system
  :defer 1
  :init
  :config
  (use-package sublimity-scroll
    :ensure nil
    :demand
    :init
    (setq sublimity-scroll-weight 6
          sublimity-scroll-drift-length 2))
  (sublimity-mode 1))

;; highlight keywords
(use-package fic-mode
  :config
    (setq fic-highlighted-words `( "TODO" "HACK" "KLUDGE" "FIXME" "TRICKY" "BUG" ))
  :init
    (me:add-hook-with-delay 'prog-mode-hook 10 #'fic-mode))

(use-package whitespace
  :config
  (setq whitespace-line-column nil                      ; highlight past fill-column
        whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab)
        whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
  :init
    (add-hook 'whitespace-mode-hook 'me:set-extra-font-attributes)
    (me:add-hook-with-delay 'prog-mode-hook 7 #'whitespace-mode)
  :diminish whitespace-mode)

;; Highlight cursor position in buffer
(use-package beacon
  :if window-system
  :defer 3
  :init
  (setq beacon-blink-when-window-scrolls nil)
  :config
  (beacon-mode 1)
  :diminish beacon-mode)

(use-package adaptive-wrap
  :config
  :init
  (setq-default adaptive-wrap-extra-indent 3)
  (me:add-hook-with-delay 'visual-line-mode-hook 3 #'adaptive-wrap-prefix-mode))

(use-package miniedit
  :general
  (:keymap minibuffer-local-map
   "C-c e" #'miniedit)
  :config
  (miniedit-mode t)
  :init)

(use-package hc-zenburn-theme
  :load-path "lisp/hc-zenburn-emacs"
  :config
  ;; make visual and highlight more noticable
  (set-face-attribute 'lazy-highlight nil :background "#5e5e5e")
  (set-face-attribute 'region nil :background "#5e5e5e")
  (set-face-attribute 'highlight nil :background "#5e5e5e")
  :defer 0)

(use-package spaceline-config
  :if window-system
  :ensure spaceline
  :defer 2
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator " ")
  :config
  (use-package powerline
    :config
    :demand
    :init
    (setq powerline-default-separator 'curve))
  (spaceline-spacemacs-theme)
  ;; Remove existing buffer local mode line format so that it uses the
  ;; global one, and then force it to update.
  (mapc (lambda (buffer)
          (if (or (buffer-file-name buffer)
                  (not (equal (substring (buffer-name buffer) 0 1) " ")))
              (with-current-buffer buffer
                ;(message "Updating %s" buffer);
                (kill-local-variable 'mode-line-format)
                (force-mode-line-update t))))
      (buffer-list)))

(use-package linum-relative
  :config
  :general
    ("<f5> l"      #'linum-relative-mode)
  :diminish linum-relative-mode
  :init
  (setq linum-relative-current-symbol ""))   ; show current line #

(use-package git-gutter-fringe+
  :if window-system
  :init
  (add-hook 'prog-mode-hook #'git-gutter+-mode)
  (add-hook 'text-mode-hook #'git-gutter+-mode)
  (add-hook 'git-gutter+-mode-hook (lambda () (diminish 'git-gutter+-mode "ⓖ")))
  :general
    ("s-g"        #'git-gutter+-next-hunk)
    ("s-S-g"      #'git-gutter+-previous-hunk)
    ("s-s"        #'git-gutter+-stage-hunks)
    ("s-o"        #'git-gutter+-show-hunk-inline-at-point)
  :config)

(use-package ruler-mode
  :general
    ("<f5> u" #'ruler-mode)
  :config)

;; better package manager
(use-package paradox
  :general
    ("<f4> P"     #'paradox-list-packages)
    (:keymaps 'paradox-menu-mode-map
              "j" #'paradox-next-entry
              "k" #'paradox-previous-entry)
  :config
  :init
  (add-hook 'paradox-menu-mode-hook (lambda ()
                                        (when (fboundp 'evil-mode)
                                          (evil-emacs-state))))
  (setq paradox-spinner-type 'moon
        paradox-execute-asynchronously nil
        paradox-display-download-count t
        paradox-display-star-count t
        paradox-github-token t                ; Dont't ask, don't integrate
        paradox-automatically-star nil        ; Don't star automatically
        paradox-hide-wiki-packages t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300) ; wait 5m before 1st cleanup
  (recentf-mode))

;; save buffer positions
(use-package saveplace
  :init
  (setq-default save-place t)
  :config
  (setq save-place-file (expand-file-name "places" user-emacs-directory)
        save-place-forget-unreadable-files nil)
  (save-place-mode t)
  :defer 3)


; modal window resizing
(use-package windresize
  :general
    ("<f5> r"      #'windresize)
    (:keymaps 'windresize-map
              "q" #'windresize-exit
              "h" #'windresize-left
              "l" #'windresize-right
              "j" #'windresize-down
              "k" #'windresize-up)
  :config)

(use-package undo-tree
  :init
  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (add-hook 'text-mode-hook 'undo-tree-mode)
  :config
  :general
    ("<f4> v"     #'undo-tree-visualize)
  :diminish undo-tree-mode)

;; remote file editting
(use-package tramp
  :ensure nil
  :init
  (setq tramp-terminal-type "dumb"                              ; avoid fancy prompts
        tramp-backup-directory-alist backup-directory-alist     ; keep backups local
        tramp-default-method "ssh")                             ; use ssh by default
)

(use-package dired
  :general
  ("<f4> d"     #'dired-jump)
  (:keymaps 'dired-mode-map
            "/" #'dired-narrow
            "C-c d" #'dired-hide-details-mode
            "C-c w" #'wdired-change-to-wdired-mode
            "C-c f" #'peep-dired)
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh"    ; human readable sizes
        dired-auto-revert-buffer t       ; revert buffer on revisit
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :config
  (use-package dired+
    :defer 2
    :config
    :init
    (setq font-lock-maximum-decoration (quote ((dired-mode . nil) (t . t)))   ; turn off barf colors
          diredp-hide-details-initially-flag t
          diredp-image-preview-in-tooltip 400
          diredp-auto-focus-frame-for-thumbnail-tooltip-flag t))

  :ensure nil)

(use-package dired-collapse
  :config
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package wdired
  :commands ( wdired-change-to-wdired-mode )
  :ensure nil
  :config
  :init
  (setq wdired-allow-to-change-permissions t))

;; Cowboy override of peep-dired-mode-map which has a
;; key define which causes an error.  Every other method
;; of doing this failed, but this works.
(defvar peep-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j")         #'peep-dired-next-file)
    (define-key map (kbd "k")         #'peep-dired-prev-file)
    (define-key map (kbd "C-f")       #'peep-dired-scroll-page-down)
    (define-key map (kbd "C-b")       #'peep-dired-scroll-page-up)
    (define-key map (kbd "q")         #'peep-dired)
    map)
  "Keymap for `peep-dired-mode'.")

(use-package peep-dired
  :init
  ;; This makes peep-dired-mode-map override all evil mappings
  ;; evil-make-overriding-map doesn't handle j and k for some reason.
  (add-hook 'peep-dired-hook (lambda ()
                                 (evil-make-intercept-map peep-dired-mode-map 'normal)
                                 (evil-normalize-keymaps 'normal)))
  :config)

(use-package dired-narrow
  :init
  :config)

(use-package flyspell
  :general
    ("s-f"        #'flyspell-auto-correct-previous-word)
    ("s-S-f"      #'flyspell-correct-previous-word-generic)
  :config
  (use-package flyspell-correct-ivy :demand)
  :init
  (setq ispell-personal-dictionary (expand-file-name "hunspell/words" me:config-directory))
  (me:add-hook-with-delay 'prog-mode-hook 10 #'flyspell-prog-mode)
  (me:add-hook-with-delay 'text-mode-hook 10 #'flyspell-mode)
  ;; setup spell check, prefer hunspell
  (cond
    ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
            ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
            ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
              )))
    ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
  :diminish (flyspell-mode . "ⓢ"))

(use-package ivy
  :general
    ("<f4> <f4>"  #'ivy-resume)
    ("<f2>"       #'ivy-switch-buffer)
    ("C-x b"      #'ivy-switch-buffer)
    (:keymaps 'ivy-minibuffer-map
              "<escape>" #'minibuffer-keyboard-quit
              "M-y" #'ivy-next-line                       ; for yank-pop flow
              "s-n" #'ivy-next-line-and-call
              "s-p" #'ivy-previous-line-and-call
              "C-c =" #'ivy-minibuffer-grow
              "C-c -" #'ivy-minibuffer-shrink
              "C-c f" #'ivy-avy)
  :config
  (ivy-mode 1)
  :init
  (add-hook 'ivy-mode-hook (lambda ()
                             (setq ivy-height (/ (+ 2 (frame-height)) 3))))
  (setq ivy-use-virtual-buffers t                           ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-virtual-abbreviate 'full                        ; use full path for abbreviation
        ivy-count-format ""                                 ; does not count candidates
        ivy-initial-inputs-alist nil                        ; no regexp by default
        ivy-re-builders-alist
           '((t . ivy--regex-ignore-order)))                ; allow input not in order
  :diminish (ivy-mode . ""))

;; add some ivy buffer information
;; N.B. Disabled until #8 reopened and fixed
(use-package ivy-rich
  :disabled
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  :defer 3)

(use-package counsel
  :commands (  counsel-file-jump counsel-find-file)
  :general
  (:keymaps 'global :prefix "<f4>"
           "a" #'counsel-apropos
           "b" #'counsel-mark-ring
           "i" #'counsel-info-lookup-symbol
           "j" #'counsel-bookmark
           "k" #'counsel-descbinds
           "l" #'counsel-locate
           "p" #'counsel-package
           "r" #'counsel-recentf
           "u" #'counsel-unicode-char)
  (:keymaps 'global :prefix "<f4> s"
           "a" #'counsel-ag
           "b" #'swiper
           "g" #'counsel-git-grep
           "s" #'swiper-all)
  ("C-h b"  #'counsel-descbinds)
  ("M-x"    #'counsel-M-x)
  ("<f5> t" #'counsel-load-theme)
  ("<f5> c" #'counsel-colors-emacs)
  ("<f5> w" #'counsel-colors-web)
  ("<f6> m" #'counsel-imenu)
  :init
  (setq counsel-yank-pop-separator "\n---\n")
  :config
  (counsel-mode 1)
  :diminish (counsel-mode . ""))

;; allow grep buffers to be editted
(use-package wgrep)

(use-package projectile
  :general
    ("<f7> k"     #'projectile-kill-buffers)
    ("<f7> o"     #'projectile-multi-occur)
    ("<f7> u"     #'projectile-invalidate-cache)
    ("<f7> n"     #'projectile-add-known-project)
    (:states '(normal visual emacs)
    :prefix "<SPC>"
         "m"    #'projectile-compile-project)
    (:prefix "C-c"
            "p"  '(:ignore t :which-key "Projectile→" ))
  :diminish projectile-mode
  :after evil
  :init
  (setq projectile-completion-system 'ivy
        projectile-globally-ignored-files #'( "TAGS" "GTAGS" "GRTAGS" "GPATH" )
        projectile-globally-ignored-file-suffixes #'( ".o" ".so" ".a" ".ko" ".jar" ".bc" ".class")
        projectile-use-git-grep t
        projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))
        projectile-enable-caching t)
  :config
  (push "compile_commands.json" projectile-project-root-files)
  (push "build" projectile-globally-ignored-directories)
  (use-package persp-projectile
    :demand
    :general
      ("<f7> <f7>"  #'projectile-persp-switch-project)
    :config)
  (projectile-mode 1))

(use-package counsel-projectile
  :general
    ("<f7> f" #'counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package perspective
  :after projectile
  :general
    ("<f7> r"     #'persp-rename)
    ("s-<right>"  #'persp-next)
    ("s-<left>"   #'persp-prev)
    (:prefix "C-x"
            "x"  '(:ignore t :which-key "Perspective→" ))
  :config
    (setq persp-initial-frame-name (projectile-project-name))
    (persp-mode))

;; Only use smart-tabs-mode if tabs are enabled
(defun me:smart-tabs-mode-unless-override ()
  (if indent-tabs-mode
      (smart-tabs-mode)))

(use-package smart-tabs-mode
  :commands (smart-tabs-mode)
  :init
  (add-hook 'c-mode-hook #'me:smart-tabs-mode-unless-override)
  (add-hook 'c++-mode-hook #'me:smart-tabs-mode-unless-override)
  (add-hook 'python-mode-hook #'me:smart-tabs-mode-unless-override)
  :config

  (smart-tabs-insinuate 'python)
  (smart-tabs-insinuate 'c 'c++))

;; Highlight delimiters by depth
(use-package rainbow-delimiters
  :init
    (me:add-hook-with-delay 'prog-mode-hook 8 #'rainbow-delimiters-mode)
  :config)

;; color color strings
(use-package rainbow-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)
  (add-hook 'js-mode-hook #'rainbow-mode)
  (add-hook 'rainbow-mode-hook (lambda () (diminish 'rainbow-mode "🌈")))
  :config)

;; Highlight cursor's surrounding parentheses
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init
  (me:add-hook-with-delay 'prog-mode-hook 8 #'highlight-parentheses-mode)
  :config)

(use-package markdown-mode
  :config
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package org
  :init
  (setq org-hide-emphasis-markers t)
  :mode
  (("\\.org\\'" . org-mode))
  :config
    (use-package evil-org :config :demand))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
  :config)

(use-package cmake-mode
  :config
  :init
  (setq cmake-tab-width 3)
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

;; n.b. buffer-face-mode screws up completion popups
;; may be fixed in 25.1 or so
(use-package adoc-mode
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case)
  :init
  (add-hook 'adoc-mode-hook
            (lambda ()
              (setq company-dabbrev-downcase nil     ; don't downcase completions
                    company-dabbrev-ignore-case nil  ; don't keep prefix
                    evil-shift-width 4               ; set tabs 4 spaces
                    tab-width 4
                    indent-tabs-mode nil)))
  :config
  (defun me:adoc-mode-flyspell-verify ()
    "flyspell function to ignore certain asciidoc markup"
    (and
      (not (save-excursion
            (let ((this (point)))
              (and
                (re-search-backward "^\\([\\.\\+-]\\{4,\\}\\|`\\{3,\\}\\)" nil t)
                (> this (point))
                (progn
                  (forward-line 1)
                  (re-search-forward (concat "^" (regexp-quote (match-string 1))) nil t))))))
      (not (save-excursion
            (let ((count 0))
              (eq 1 (progn (while (re-search-backward "`" (line-beginning-position) t)
                              (setq count (1+ count)))
                            (- count (* 2 (/ count 2))))))))))
  (put 'adoc-mode 'flyspell-mode-predicate #'me:adoc-mode-flyspell-verify)
  :mode
  (("\\.ad\\'" . adoc-mode)
   ("\\.adoc\\'" . adoc-mode)
   ("\\.asciidoc\\'" . adoc-mode)))

(use-package python-mode
  :defines ( python-indent-offset python-indent-guess-indent-offset )
  :config
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (semantic-mode t)
              (setq evil-shift-width 4)
              (setq python-indent-offset 4)
              (setq python-indent-guess-indent-offset t)
              (setq tab-width 4)))
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.py3\\'" . python-mode)))

(use-package ruby-mode
  :config
  :mode
  (("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
   ("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package js2-mode
  :config
  :mode
  (("\\.js\\'" . js-mode)
   ("\\.json\\'" . js-mode)))

;; basic setup for java code
(use-package java-mode
  :ensure nil
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (setq c-basic-offset 4                             ; use common convention
                    tab-width 8                                  ; 4 spaces indentation
                    evil-shift-width 4                           ; no tabs
                    indent-tabs-mode nil)))
  :config
  :mode
  (("\\.java\\'" . java-mode)))

(use-package cc-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq c-basic-offset 3)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (define-key c++-mode-map ":" #'self-insert-command)
              (define-key c++-mode-map ")" #'self-insert-command)
              (define-key c++-mode-map ";" #'self-insert-command)
              (c-set-offset 'arglist-intro '++)               ; indent args extra
              (c-set-offset 'innamespace [0])))              ; no indentation in namespace
  (setq c-default-style "ellemtel"                           ; similar to allman style
        c-electric-pound-behavior (quote (alignleft))        ; cpp directives aligned to left
        show-paren-mode 0)
  :config
  :mode
  (("\\.c\\'"   . c-mode)
   ("\\.cpp\\'" . c++-mode)
   ("\\.cxx\\'" . c++-mode)
   ("\\.h\\'"   . c++-mode)
   ("\\.hpp\\'" . c++-mode)))

;; very large file support
(use-package vlf
  :general
  (:prefix "C-c"
           "C-v"  '(:ignore t :which-key "VLF→" ))
  :defer 0
  :init
  (require 'vlf-setup)
  :config)

(use-package logview)

;; font lock for newer c++ versions
(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :config
  :init
  (me:add-hook-with-delay 'c++-mode-hook 10 #'modern-c++-font-lock-mode))


(use-package compile
  :commands compile
  :general
    (:states '(normal visual emacs)
    :prefix "<SPC>"
      "o"          #'me:switch-to-compile-buffer
      "r"          #'recompile)
    (:keymaps 'compilation-mode-map
              "<up>" #'compilation-previous-error
              "<down>" #'compilation-next-error
              "<prior>" #'compilation-previous-file
              "<next>" #'compilation-next-file
              "C-c C-e" #'me:rotate-skip-threshold
              "<SPC>" nil
              "g" nil
              "j" nil
              "k" nil
              "h" nil
              "l" nil)
  :config
  (defun me:switch-to-compile-buffer ()
    (interactive)
    (switch-to-buffer "*compilation*"))
  (defun me:rotate-skip-threshold ()
    (interactive)
    (compilation-set-skip-threshold
      (cond ((= compilation-skip-threshold 1) 2)
            ((= compilation-skip-threshold 2) 0)
            (t 1))))
  :init
  (setq compilation-scroll-output t
        compilation-ask-about-save nil    ; save all modified
        compilation-auto-jump-to-first-error t
        compilation-finish-functions (lambda (_buf str)
          (compilation-set-skip-threshold 1)
          (if (null (string-match ".*exited abnormally.*" str))
              ;;if no errors, make the compilation window go away in a few seconds
              (progn
                (run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create "*compilation*"))
                (message "No Compilation Errors!"))
        compilation-skip-threshold 2)))
  (add-hook 'compilation-start-hook
            (lambda (_proc) (compilation-set-skip-threshold 2)))

  (add-hook 'compilation-mode-hook (lambda ()
                                        (when (fboundp 'evil-mode)
                                          (evil-make-intercept-map compilation-mode-map 'normal)
                                          (evil-normalize-keymaps)))))

;; view symbols of libraries
(use-package elf-mode
  :init
  :magic
  (("ELF" . elf-mode))
  :config)

(use-package woman
  :ensure nil
  :init
  (setq woman-use-topic-at-point t                          ; man page on word at point if exists
        Man-notify-method 'aggressive)                      ; show&select man page in other window
  :general
    ("<f4> m" #'woman)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

(use-package doc-view
  :ensure nil
  :config
  :init
  (setq doc-view-continuous t
        doc-view-resolution 144)
  :general
  (:keymaps 'doc-view-mode-map
            "j" #'doc-view-next-line-or-next-page
            "k" #'doc-view-previous-line-or-previous-page
            "h" #'image-backward-hscroll
            "l" #'image-forward-hscroll))

(use-package deft
  :config
  (if (file-readable-p "~/Dropbox/Notes")
      (setq deft-directory "~/Dropbox/Notes"))
  :general
    ("<f4> n"     #'deft)
  :init
  (setq deft-directory "~/Notes"   ; can be overridden in config
        deft-recursive t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((nospace . "_")
                                  (noslash . "_")
                                  (case-fn . downcase))
        ; first extension in list seems to be used for new files.  Not sure
        ; what deft-default-extesion does
        deft-default-extension "md"
        deft-extensions '("md" "txt" "text" "markdown" "mmd" "org")
                                      ; deft auto-save interferes with whitespace-butler, so disable
        deft-auto-save-interval 0)
  (add-hook 'deft-mode-hook
            (lambda ()
              (define-key deft-mode-map (kbd "<f4> n") #'quit-window)
              (define-key deft-mode-map (kbd "<C-return>") #'deft-new-file)
              (define-key deft-mode-map (kbd "<C-backspace>") #'deft-filter-clear))))

(use-package company
  :general
    ("s-d"        #'company-complete)
  :init
  (setq company-minimum-prefix-length 1            ; just one char needed
        company-dabbrev-downcase nil)              ; never downcase
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'company-completion-started-hook #'me:company-started)
  (add-hook 'company-completion-finished-hook #'me:company-ended)
  (add-hook 'company-completion-cancelled-hook #'me:company-ended)
  :config
  (defvar me:flycheck-error-function nil)
  (defun me:company-started (&optional _args)
    (when (fboundp 'flycheck-pos-tip-error-messages)
      (setq me:flycheck-error-function (symbol-function 'flycheck-pos-tip-error-messages))
      (fset 'flycheck-pos-tip-error-messages 'ignore)))

  (defun me:company-ended (&optional _args)
    (when (fboundp 'flycheck-pos-tip-error-messages)
      (fset 'flycheck-pos-tip-error-messages me:flycheck-error-function)))
  (use-package company-quickhelp
    :demand
    :config
    (company-quickhelp-mode 1))
  :diminish company-mode "ⓒ")

(use-package counsel-gtags
  :commands ( counsel-gtags-find-definition
              counsel-gtags-find-reference
              counsel-gtags-find-file
              counsel-gtags-dwim
              counsel-gtags-create-tags
              counsel-gtags-find-symbol)
  :init
  (add-hook 'c-mode-hook #'counsel-gtags-mode)
  (add-hook 'c++-mode-hook #'counsel-gtags-mode)
  :config
  :diminish counsel-gtags-mode)

(use-package rtags
  :init
  (defun me:flycheck-rtags-setup ()
    (require 'flycheck-rtags)
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))
  (defun me:company-rtags-setup ()
    (require 'company-rtags)
    (add-to-list 'company-backends 'company-rtags))
  (defun me:use-rtags (&optional useFileManager)
    (and (rtags-executable-find "rc")
         (cond
          ((not (counsel-gtags--root)) t)
          ((and (not (eq major-mode 'c++-mode))
                (not (eq major-mode 'c-mode)))
           (rtags-has-filemanager))
          (useFileManager (rtags-has-filemanager))
          (t (rtags-is-indexed)))))
  (defun me:tags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (not (rtags-find-symbol-at-point prefix))
          (counsel-gtags-dwim)))
  (defun me:tags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (not (rtags-find-references-at-point prefix))
          (counsel-gtags-dwim)))
  (defun me:tags-find-symbol ()
    (interactive)
    (call-interactively (if (me:use-rtags) #'rtags-find-symbol #'counsel-gtags-find-symbol)))
  (defun me:tags-find-file ()
    (interactive)
    (call-interactively (if (me:use-rtags t) #'rtags-find-file #'counsel-gtags-find-file)))
  (defun me:tags-find-file ()
    (interactive)
    (call-interactively (if (me:use-rtags t) #'rtags-find-file #'counsel-gtags-find-file)))
  (setq rtags-autostart-diagnostics t
        rtags-display-result-backend 'ivy
        rtags-tooltips-enabled nil
        rtags-display-current-error-as-message nil
        rtags-completions-enabled t)
  (add-hook 'c++-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'c-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook #'me:flycheck-rtags-setup)
  (add-hook 'c-mode-hook #'me:flycheck-rtags-setup)
  (add-hook 'company-mode-hook  #'me:company-rtags-setup)
  :general
    ("s-;"        #'rtags-location-stack-forward)
    ("s-,"        #'rtags-location-stack-back)
    ("<f6> c"     #'rtags-rename-symbol)
    ("<f6> d"     #'me:tags-find-symbol)
    ("<f6> f"     #'me:tags-find-file)
    ("<f6> i"     #'rtags-find-functions-called-by-this-function)
    ("<f6> r"     #'me:tags-find-references-at-point)
    ("<f6> v"     #'rtags-find-virtuals-at-point)
    ("<f6> <RET>"  #'rtags-create-doxygen-comment)
    ("<f6> <f6>"  #'me:tags-find-symbol-at-point)
  :config
  (rtags-diagnostics)
  (rtags-set-periodic-reparse-timeout 2)
  (rtags-enable-standard-keybindings))

;; yas snippets
(use-package yasnippet
  :commands ( yas-expand-snippet )
  :general
    ("<s-return>" #'yas-expand)
    (:prefix "C-c"
            "&" '(:ignore t :which-key "Yasnippet→" ))
  :init
  (add-hook 'yas-before-expand-snippet-hook         ; evil-insert at each slot
            (lambda()
                  (let ((p (point)) (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m))))
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil) ; don't use <tab>
  (define-key yas-minor-mode-map (kbd "TAB") nil)   ; don't use TAB
  :diminish (yas-minor-mode . "ⓨ"))


(use-package flycheck
  :diminish ( flycheck-mode . "🗹")
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (me:add-hook-with-delay 'prog-mode-hook 10 #'flycheck-mode)
  :general
  (:prefix "C-c"
           "!"  '(:ignore t :which-key "Flycheck→" ))
  :config)

(use-package flycheck-pos-tip
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)
  :config
  (setq flycheck-pos-tip-timeout 3))

;; enable code folding (evil has bindings)
(use-package hideshow
  :general
  (:prefix "C-c"
           "@" '(:ignore t :which-key "HideShow→" ))
  :config
  :init
  (me:add-hook-with-delay 'prog-mode-hook   5 #'hs-minor-mode)
  :diminish (hs-minor-mode . "ⓕ"))

(use-package avy
  :commands ( avy-goto-word-1 avy-goto-char-2 avy-goto-char-in-line )
  :config
  :init
  (setq avy-all-windows 'all-frames))

(use-package evil
  :init
  (add-hook 'prog-mode-hook #'evil-mode)
  (add-hook 'text-mode-hook #'evil-mode)
  ;; make cut/paste more vim-like
  ;; mainly keep emacs cut/paste separate from system clipboard
  ;; in the default case (must use "+ or "* to override)
  ;; This assumes select-enable-clipboard is set to nil as well
  (add-hook 'evil-local-mode-hook (lambda ()
                                (setq-default interprogram-paste-function nil
                                              interprogram-cut-function nil)))
  (setq-default evil-symbol-word-search t   ; misnamed: t is search for symbols, not words
                evil-shift-width 3)         ; shift by ideal width :)
  (setq evil-want-C-w-delete nil            ; want C-w it for windows commands
        evil-want-C-w-in-emacs-state t      ; ditto
        evil-want-C-i-jump nil              ; need TAB for other things
        evil-indent-convert-tabs nil        ; make = work with smart tabs mode
        evil-search-module #'evil-search)
  :general
    (:states '(normal visual emacs)
    :prefix "<SPC>"
      "<SPC>"      #'me:switch-to-previous-buffer
      ";"          #'evil-jump-forward
      ","          #'evil-jump-backward
      "a"          #'align
      "f"          #'evil-avy-goto-word-1
      "g"          #'evil-avy-goto-char-2
      "l"          #'evil-avy-goto-char-in-line
      "w"          #'save-buffer
      "x"          #'exchange-point-and-mark
      "<DEL>"      #'kill-this-buffer)
    (:states '(normal visual) :prefix "<SPC>"
             "s"  (general-simulate-keys "\"*")
             "c"  (general-simulate-keys "\"+"))
    (:keymaps '(normal visual ) "<escape>" #'keyboard-quit)

    ;; Move via visual lines
    (:keymaps 'normal "j"   #'evil-next-visual-line)
    (:keymaps 'normal "k"   #'evil-previous-visual-line)

    ;; Scroll keeping cursor stationary
    (:keymaps '( normal insert visual ) "C-j" #'evil-scroll-line-up)       ; ^y
    (:keymaps '( normal insert visual )  "C-k" #'evil-scroll-line-down)     ; ^e

    ;; Overload shifts so that they don't lose the selection
    (:keymaps 'visual
              ">"           #'me:evil-shift-right-visual
              "<tab>"       #'me:evil-shift-right-visual)
    (:keymaps 'visual
              "<"           #'me:evil-shift-left-visual
              "<backtab>"   #'me:evil-shift-left-visual)

  :config

  (defun me:evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun me:evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun me:switch-to-previous-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  (use-package evil-args
    :general
    (:keymaps 'inner "a"  #'evil-inner-arg)
    (:keymaps 'outer "a"  #'evil-outer-arg)
    :config )

  (use-package evil-textobj-anyblock
    :general
    (:keymaps 'inner "b"  #'evil-textobj-anyblock-inner-block)
    (:keymaps 'outer "b"  #'evil-textobj-anyblock-a-block)
    :config )

  (use-package evil-commentary
    :general
    (:keymaps 'motion "gc" #'evil-commentary)
    (:keymaps 'motion "gy" #'evil-commentary-yank)
    :config
    (evil-commentary-mode)
    :diminish evil-commentary-mode)

  ;; want to start deft in insert mode
  (evil-set-initial-state 'deft-mode 'insert)

  ;; these modes are better in emacs state
  (dolist (hook '(git-rebase-mode-hook
                  flycheck-error-list-mode-hook
                  rtags-mode-hook
                  finder-mode-hook
                  doc-view-mode-hook
                  image-mode-hook
                  image-dired-thumbnail-mode-hook))
           (add-hook hook #'evil-emacs-state))

  ;; Put view-mode in motion-state, this gives us motion
  ;; keys and not much more, this is good for read-only scenario
  ;; Since view-mode is combined with other modes, this needs
  ;; to be a hook.
  (add-hook 'view-mode-hook (lambda () (if view-mode (evil-motion-state) (evil-normal-state))))

  (evil-mode 1))

(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode)
  :config
  (setq ws-butler-convert-leading-tabs-or-spaces t)       ; convert according to indent-tabs-mode (but not when smart-tabs-mode on)
  :diminish ws-butler-mode "✅")

(use-package shell-pop
  :config
  :general
    ("<f4> t"     #'shell-pop)
  :init
  (setq shell-pop-internal-mode "ansi-term"
        shell-pop-term-shell "/bin/bash"
        shell-pop-window-size 40
        shell-pop-window-position "bottom"
        shell-pop-universal-key "<f4> t"))

(use-package which-key
  :init
  :config
  (setq which-key-max-description-length nil
        which-key-allow-evil-operators t)
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  :defer 2
  :diminish which-key-mode)

(use-package with-editor
  :init
  (add-hook 'with-editor-mode-hook (lambda ()
                                     (evil-insert-state)
                                     (setq fill-column 70)))
  :diminish)

(use-package magit
  :after evil
  :init
  (setq magit-completing-read-function 'ivy-completing-read    ; use ivy
        vc-handled-backends nil)                               ; magit does everything needed
  :general
    ("<f9> b"     #'magit-blame)
    ("<f9> B"     #'magit-run-git-gui-blame)
    ("<f9> c"     #'magit-commit)
    ("<f9> a"     #'magit-commit-amend)
    ("<f9> i"     #'git-gutter+-show-hunk)
    ("<f9> l"     #'magit-log-current)
    ("<f9> f"     #'magit-log-buffer-file)
    ("<f9> <f9>"  #'magit-status)
  :config)

(use-package evil-magit
    :after magit
    :init
    (setq evil-magit-state 'normal))

;; Cowboy override of git-timemachine-mode-map
;; Timemachine's map has a number of bindings which
;; are evil commands.  So we clear it out here and
;; define our own bindings using general.
(defvar git-timemachine-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `git-timemachine-mode-map'.")

(use-package git-timemachine
  :init
  ;; Use timemachine map on top of evil bindings
  (add-hook 'git-timemachine-mode-hook (lambda ()
                                           (when (fboundp 'evil-mode)
                                              (evil-make-overriding-map git-timemachine-mode-map 'normal)
                                              (evil-normalize-keymaps 'normal))))
  :config
  :general
  (:keymaps 'global :prefix "<f9>"  "t" #'git-timemachine)
  (:keymaps 'git-timemachine-mode-map
            "s-p" #'git-timemachine-show-previous-revision
            "s-n" #'git-timemachine-show-next-revision
            "q" #'git-timemachine-quit   ; we lose q command in evil, but won't matter really
            "C-c g" #'git-timemachine-show-nth-revision
            "C-c a"  #'git-timemachine-kill-abbreviated-revision
            "C-c r"  #'git-timemachine-kill-revision
            "C-c b" #'git-timemachine-blame)
  :diminish "🕓")

(use-package treemacs
  :general
    ("<f4> /"   #'treemacs-toggle)
    ("<f7> /"   #'treemacs-projectile)
  :init
  (setq treemacs-header-function            #'treemacs-projectile-create-header
        treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-silent-refresh             t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-collapse-dirs              3
        treemacs-never-persist              nil)
  :config
  (use-package treemacs-evil :demand t)
  (use-package treemacs-projectile :demand t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package popwin
  :init
  :config
  (push '("*Async Shell Command*" :noselect t) popwin:special-display-config)
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
  (push '("*Help*" :stick t ) popwin:special-display-config)
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)
  (push '("*undo-tree*" :stick t :width 60 :position right) popwin:special-display-config)
  (push '("*General Keybindings*" :width 72 :position right) popwin:special-display-config)
  (popwin-mode 1)
  :defer 2)

;; Load system-dependent init file if it exists
;; will be in emacs.d/init-<prefix>-<ident>.el
(defun me:load-init-file (prefix ident)
  (let ((file-name (expand-file-name (concat "init-" prefix "-" (me:replace-all ident "/" "-") ".el") user-emacs-directory)))
  ;; (message "looking for %s" file-name)
  (when (file-exists-p file-name)
    ;; (message "loading %s" file-name)
    (load-file file-name))))

(me:load-init-file "system" (symbol-name system-type))
(me:load-init-file "host" (system-name))
