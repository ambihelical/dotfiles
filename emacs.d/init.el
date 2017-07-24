;;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)

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

(eval-after-load "use-package"
  '(setq use-package-always-ensure t                              ; ensure by default
         use-package-always-defer t                               ; defer by default
         use-package-enable-imenu-support t                       ; support for packages in imenu
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
        (set-frame-font "Hack-11:autohint=true" t t)
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
  (setq auto-revert-check-vc-info nil                         ; don't update branch on auto-revert
        auto-revert-verbose nil)                              ; don't tell me about auto reverts
  :config
  (global-auto-revert-mode t)                                 ; revert unchanged files automatically
  :diminish auto-revert-mode)

;; built-in "simple" package
(use-package simple
  :ensure nil
  :general
    ("s-n"        #'next-error)
    ("s-p"        #'previous-error)
  :init
  (setq kill-ring-max 200                      ; More killed items
        kill-do-not-save-duplicates t          ; No duplicates in kill ring
        save-interprogram-paste-before-kill t  ; save clipboard before killing
        visual-line-fringe-indicators
           '(left-curly-arrow nil))            ; use left curly error for wrapped lines
  :config
  (column-number-mode t)                       ; display column/row of cursor in mode-line
  (global-visual-line-mode t)                  ; wrap long lines
  :diminish visual-line-mode
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
  (setq sublimity-scroll-weight 6
        sublimity-scroll-drift-length 2)
  (run-with-idle-timer 1 nil (lambda ()
    (require 'sublimity-scroll)))
  :config
  (sublimity-mode 1))

;; highlight keywords
(use-package fic-mode
  :commands fic-mode
  :config
  :init
    (setq fic-highlighted-words `( "TODO" "HACK" "KLUDGE" "FIXME" "TRICKY" "BUG" ))
    (me:add-hook-with-delay 'prog-mode-hook 10 #'fic-mode))

(use-package whitespace
  :commands whitespace-mode
  :config
  :init
    (setq whitespace-line-column nil                      ; highlight past fill-column
          whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab)
          whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
    (add-hook 'whitespace-mode-hook 'me:set-extra-font-attributes)
    (me:add-hook-with-delay 'prog-mode-hook 7 #'whitespace-mode)
  :diminish whitespace-mode)

;; N.B. Disabled because it seems to interfere with popups
(use-package fill-column-indicator
  :disabled
  :config
    (add-hook 'after-change-major-mode-hook (lambda () (if buffer-file-name (fci-mode 1))))
    (setq fci-rule-color "white smoke"))

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
  :commands adaptive-wrap-prefix-mode
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

(use-package leuven-theme :defer 0 :config)

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
    ("<f4> l"      #'linum-relative-mode)
  :diminish linum-relative-mode
  :init
  (setq linum-relative-current-symbol ""))   ; show current line #

(use-package git-gutter-fringe+
  :if window-system
  :init
  :general
    ("s-g"        #'git-gutter+-next-hunk)
    ("s-S-g"      #'git-gutter+-previous-hunk)
    ("s-s"        #'git-gutter+-stage-hunks)
    ("s-o"        #'git-gutter+-show-hunk-inline-at-point)
  :config
  (global-git-gutter+-mode t)
  :defer 1
  :diminish git-gutter+-mode)

(use-package ruler-mode
  :general
    ("<f4> r" #'ruler-mode)
  :config)

;; better package manager
(use-package paradox
  :general
    ("<f4> p"     #'paradox-list-packages)
  :config
  :init
  (setq paradox-spinner-type 'moon
        paradox-execute-asynchronously nil
        paradox-display-download-count t
        paradox-display-star-count t
        paradox-github-token t                ; Dont't ask, don't integrate
        paradox-automatically-star nil        ; Don't star automatically
        paradox-hide-wiki-packages t))

(use-package recentf
  :config (recentf-mode)
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300))           ; wait 5m before 1st cleanup

;; save buffer positions
(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" user-emacs-directory)
        save-place-forget-unreadable-files nil)
  :config
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
  :config
  (global-undo-tree-mode)
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
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :config
    (use-package peep-dired
      :config
      :general
      (:keymaps 'dired-mode-map
                "C-f" #'peep-dired))

    (use-package dired-narrow
      :config
      :general
      (:keymaps 'dired-mode-map
                  "/" #'dired-narrow))

    (use-package dired+
      :defer 2
      :config
      :init
      (setq font-lock-maximum-decoration (quote ((dired-mode . nil) (t . t)))   ; turn off barf colors
            diredp-hide-details-initially-flag t
            diredp-image-preview-in-tooltip 400
            diredp-auto-focus-frame-for-thumbnail-tooltip-flag t))
  :ensure nil)

(use-package flyspell
  :commands ( flyspell-prog-mode flyspell-mode )
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
    (:keymaps 'ivy-mode-map
              "<escape>" #'minibuffer-keyboard-quit
              "C-j" #'ivy-next-line-and-call
              "C-k" #'ivy-previous-line-and-call
              "C-=" #'ivy-minibuffer-grow
              "C--" #'ivy-minibuffer-shrink
              "C-'" #'ivy-avy)
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
    ("<f4> a" #'counsel-apropos)
    ("<f4> b" #'counsel-mark-ring)
    ("<f4> f" #'counsel-git-grep)
    ("<f4> i" #'counsel-info-lookup-symbol)
    ("<f4> j" #'counsel-bookmark)
    ("<f4> k" #'counsel-descbinds)
    ("<f4> l" #'counsel-locate)
    ("<f4> r" #'counsel-recentf)
    ("<f4> s" #'counsel-ag)
    ("<f4> u" #'counsel-unicode-char)
    ("C-h b"  #'counsel-descbinds)
    ("M-x"    #'counsel-M-x)
    ("M-y"    #'counsel-yank-pop)
    ("<f5> t" #'counsel-load-theme)
    ("<f6> m" #'counsel-imenu)
    (:keymaps 'ivy-minibuffer-map
              "M-y" #'ivy-next-line)
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
        projectile-project-root-files-functions
          #'( projectile-root-local projectile-root-top-down projectile-root-bottom-up projectile-project-root-files-top-down-recurring)
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
    ("<f7> f" #'counsel-projectile-find-file)
  :config
  ;; version of counsel-projectile-find-file which uses
  ;; projectile-current-project-files instead of the fontified
  ;; counsel-projectile--file-list
  (defun counsel-projectile-find-file (&optional arg)
    (interactive "P")
    (projectile-maybe-invalidate-cache arg)
    (ivy-read (projectile-prepend-project-name "Find file: ")
              (projectile-current-project-files)
              :matcher #'counsel--find-file-matcher
              :require-match t
              :keymap counsel-projectile-map
              :action #'counsel-projectile--find-file-action
              :caller 'counsel-projectile-find-file))
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

(use-package smart-tabs-mode
  :commands (smart-tabs-mode)
  :init
  (add-hook 'c-mode-hook #'smart-tabs-mode)
  (add-hook 'c++-mode-hook #'smart-tabs-mode)
  (add-hook 'python-mode-hook #'smart-tabs-mode)
  :config
  (smart-tabs-insinuate 'python)
  (smart-tabs-insinuate 'c 'c++))

;; Highlight delimiters by depth
(use-package rainbow-delimiters
  :init
    (me:add-hook-with-delay 'prog-mode-hook 8 #'rainbow-delimiters-mode)
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
        compilation-finish-functions
        (lambda (buf str)
          (compilation-set-skip-threshold 1)
          (if (null (string-match ".*exited abnormally.*" str))
              ;;if no errors, make the compilation window go away in a few seconds
              (progn
                (run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create "*compilation*"))
                (message "No Compilation Errors!"))
        compilation-skip-threshold 2)))
  (add-hook 'compilation-start-hook
            (lambda (proc) (compilation-set-skip-threshold 2)))
  (add-hook 'compilation-mode-hook
    (lambda ()
      (define-key compilation-mode-map (kbd "g") nil)
      (define-key compilation-mode-map (kbd "j") nil)
      (define-key compilation-mode-map (kbd "k") nil)
      (define-key compilation-mode-map (kbd "h") nil)
      (define-key compilation-mode-map (kbd "l") nil)
      (define-key compilation-mode-map (kbd "<spc>") nil)
      (define-key compilation-mode-map (kbd "<tab>") #'me:rotate-skip-threshold)
      (define-key compilation-mode-map (kbd ";") #'next-error-follow-minor-mode)
      (define-key compilation-mode-map (kbd "<up>") #'compilation-previous-error)
      (define-key compilation-mode-map (kbd "<down>") #'compilation-next-error)
      (define-key compilation-mode-map (kbd "<prior>") #'compilation-previous-file)
      (define-key compilation-mode-map (kbd "<next>") #'compilation-next-file))))

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
  :commands ( global-company-mode)
  :general
    ("s-d"        #'company-complete)
  :init
  (setq company-minimum-prefix-length 1            ; just one char needed
        company-dabbrev-downcase nil)              ; never downcase
  :config
  (use-package company-quickhelp
    :demand
    :config
    (company-quickhelp-mode 1))
  (global-company-mode)
  :diminish company-mode)

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
                    (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
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
  :init
  (setq yas-fallback-behavior 'return-nil)          ; don't try old binding
  (add-hook 'yas-before-expand-snippet-hook         ; evil-insert at each slot
            #'(lambda()
                  (let ((p (point)) (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m))))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil) ; don't use <tab>
  (define-key yas-minor-mode-map (kbd "TAB") nil)   ; don't use TAB
  (yas-global-mode)
  :diminish (yas-minor-mode . "Ⓨ"))

(use-package flycheck
  :commands flycheck-mode
  :diminish ( flycheck-mode . "ⓒ")
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (me:add-hook-with-delay 'prog-mode-hook 10 #'flycheck-mode)
  :general
  (:prefix "C-c"
           "!"  '(:ignore t :which-key "Flycheck→" ))
  :config
  (use-package flycheck-pos-tip
    :demand
    :init
    (progn
      (setq flycheck-pos-tip-timeout 3))
    :config
    (progn
      (flycheck-pos-tip-mode t)))
  (set-face-attribute 'flycheck-warning nil :foreground 'unspecified :background "khaki1")
  (set-face-attribute 'flycheck-error nil :foreground 'unspecified :background "light pink"))

;; enable code folding (evil has bindings)
(use-package hideshow
  :commands hs-minor-mode
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
  :commands evil-mode
  :init
  (add-hook 'prog-mode-hook #'evil-mode)
  (add-hook 'text-mode-hook #'evil-mode)
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
      "c"          #'me:use-evil-clipboard-register
      "e"          #'pp-eval-last-sexp
      "f"          #'evil-avy-goto-word-1
      "g"          #'evil-avy-goto-char-2
      "l"          #'evil-avy-goto-char-in-line
      "s"          #'me:use-evil-selection-register
      "w"          #'save-buffer
      "x"          #'exchange-point-and-mark
      "<DEL>"      #'kill-this-buffer)
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

  (defun me:use-evil-selection-register ()
    (interactive)
    (evil-execute-macro 1 "\"*"))

  (defun me:use-evil-clipboard-register ()
    (interactive)
    (evil-execute-macro 1 "\"+"))

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
                  image-dired-thumbnail-mode-hook
                  paradox-menu-mode-hook
                  magit-branch-manager-mode-hook
                  magit-log-mode-hook
                  magit-revision-mode-hook))
           (add-hook hook #'evil-emacs-state))

  ;; Put view-mode in motion-state, this gives us motion
  ;; keys and not much more, this is good for read-only scenario
  ;; Since view-mode is combined with other modes, this needs
  ;; to be a hook.
  (add-hook 'view-mode-hook (lambda () (if view-mode (evil-motion-state) (evil-normal-state))))

  (evil-mode 1))

(use-package ws-butler
  :init
  (setq ws-butler-convert-leading-tabs-or-spaces t)       ; convert according to indent-tabs-mode (but not when smart-tabs-mode on)
  :config
  (ws-butler-global-mode t)
  :diminish ws-butler-mode
  :defer 3)

(use-package shell-pop
  :config
  :general
    ("<f4> t"     #'shell-pop)
  :init
  (setq shell-pop-internal-mode "ansi-term"
        shell-pop-term-shell "/bin/bash"
        shell-pop-window-size 40
        shell-pop-window-position "top"
        shell-pop-universal-key "<f4> t"))

(use-package which-key
  :init
  (setq which-key-max-description-length nil)
  :config
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
  :config
  (use-package evil-magit
    :demand
    :init
    (setq evil-magit-state 'normal)))

(use-package treemacs
  :general
    ("<f4> /"   #'treemacs-toggle)
    ("<f7> /"   #'treemacs-projectile)
  :init
  (setq treemacs-header-function            #'treemacs--create-header-projectile
        treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil)
  :config
  (use-package treemacs-evil :demand t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package popwin
  :init
  (setq display-buffer-function #'popwin:display-buffer)
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
(me:load-init-file "host" system-name)

(setq gc-cons-threshold 800000)
