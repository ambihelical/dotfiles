(setq gc-cons-threshold 100000000)

;; package management

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/"))
      use-package-always-ensure t                              ; ensure by default
      use-package-always-defer t                               ; defer by default
      use-package-enable-imenu-support t                       ; support for packages in imenu
      use-package-minimum-reported-time 0.03                   ; minimum time when verbose
      use-package-verbose nil)                                 ; don't be verbose
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package )
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

;; set some personal variables
(defconst me:data-directory (or (getenv "XDG_DATA_HOME") "~/.local/share"))
(defconst me:cache-directory (or (getenv "XDG_CACHE_HOME")  "~/.cache"))
(defconst me:config-directory (or (getenv "XDG_CONFIG_HOME")  "~/.config"))
(defconst me:emacs-backup-directory (expand-file-name "emacs" me:cache-directory))
(defconst me:notes-path (if (file-readable-p "~/Dropbox/Notes")
                            "~/Dropbox/Notes"
                          "~/Notes") "Location of note files")

;; replace prefix part of a string
(defun me:replace-prefix (prefix input)
  (replace-regexp-in-string ( concat "^" (regexp-quote prefix)) "" input))

;; replace any matches in a string
(defun me:replace-all (input from to)
  (replace-regexp-in-string (regexp-quote from) to input nil))


;; configuration I haven't figured out how to wedge into
;; use-package

(setq-default tab-width 3                                   ; ideal tab width
              indent-tabs-mode t                            ; enable tabs for most files
              indicate-empty-lines t                        ; show empty lines at end of buffer
              fill-column 120)                              ; auto-wrap only very long lines
(setq auto-revert-check-vc-info nil                         ; don't update branch on auto-revert
      auto-revert-verbose nil                               ; don't tell me about auto reverts
      auto-save-file-name-transforms
         `((".*" ,me:emacs-backup-directory t))             ; autosave files in backup directory
      backup-directory-alist
         `((".*" . ,me:emacs-backup-directory))             ; backup files in backup directory
      custom-file "/dev/null"                               ; disable customizations
      enable-recursive-minibuffers t                        ; allow recursive edit
      fast-but-imprecise-scrolling t                        ; quick and dirty scrolling
      frame-title-format
        '((:eval (if (buffer-file-name)
                    (me:replace-prefix (abbreviate-file-name default-directory)
                                        (abbreviate-file-name buffer-file-name))
                  "%b"))
          " %* ["
          (:eval (abbreviate-file-name default-directory))
          "]")                                              ; fancy title
      history-length 1000                                   ; length of history
      icon-title-format frame-title-format                  ; use same title for unselected frame
      inhibit-splash-screen t                               ; no splash
      inhibit-startup-echo-area-message t                   ; no startup message
      inhibit-startup-message t                             ; no startup message
      initial-major-mode 'text-mode                         ; no prog-mode at startup
      initial-scratch-message nil                           ; no scratch message
      kill-ring-max 200                                     ; More killed items
      kill-do-not-save-duplicates t                         ; No duplicates in kill ring
      load-prefer-newer t                                   ; load source if newer than bytecode
      mouse-wheel-scroll-amount '(3 ((shift) . 9))          ; 3 lines, or 9 line when shift held
      mouse-wheel-follow-mouse 't                           ; scroll window under mouse
      mouse-wheel-progressive-speed nil                     ; don't speed up
      undo-limit 1000000                                    ; 1M (default is 80K)
      undo-strong-limit 1500000                             ; 1.5M (default is 120K)
      undo-outer-limit 150000000                            ; 150M (default is 12M)
      save-interprogram-paste-before-kill t                 ; save clipboard before killing
      scroll-margin 5                                       ; show some lines around cursor when possible
      select-enable-clipboard nil                           ; make cut/paste function correctly
      sentence-end-double-space nil                        ; sentences end with one space
      split-width-threshold 240                             ; 2x ideal line width :)
      standard-indent 3                                     ; ideal indent :)
      text-scale-mode-step 1.05                             ; text size increases by 5% (normally 20%)
      vc-handled-backends nil                               ; magit does everything needed
      view-read-only t                                      ; show r/o files in view mode
      visual-line-fringe-indicators '(left-curly-arrow nil) ; use left curly error for wrapped lines
      x-gtk-use-system-tooltips nil)                        ; allow tooltip theming

(add-hook 'focus-out-hook #'me:save-dirty-buffers)          ; save on defocus
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

;; set font in order of preference
(if (member "Hack" (font-family-list))
    (set-frame-font "Hack-11:autohint=true" t t)
  (if (member "Fantasque Sans Mono" (font-family-list))
      (set-frame-font "Fantasque Sans Mono-12" t t)
    (if (member "DejaVu Sans Mono" (font-family-list))
        (set-frame-font "DejaVu Sans Mono-11" t t))))

(tool-bar-mode 0)                                           ; no tool bar
(scroll-bar-mode 0)                                         ; no scroll bar
(menu-bar-mode 0)                                           ; no menu bar
(mouse-avoidance-mode 'animate)                             ; move mouse pointer out of way
(column-number-mode t)                                      ; display column/row of cursor in mode-line
(global-hl-line-mode t)                                     ; highlight current line
(global-eldoc-mode -1)                                      ; turn off annoying eldoc mode
(fset 'yes-or-no-p 'y-or-n-p)                               ; change stupid default y/n? y
(make-directory me:emacs-backup-directory t)                ; make sure backup dir exists
(global-auto-revert-mode t)                                 ; revert unchanged files automatically
(electric-indent-mode +1)                                   ; turn on electric mode globally
(global-visual-line-mode t)                                 ; wrap long lines
(savehist-mode t)                                           ; save minibuffer history
(winner-mode t)                                             ; enable winner mode
(minibuffer-depth-indicate-mode t)                          ; show recursive edit depth
(run-at-time "1 hour" 3600 #'clean-buffer-list)             ; clear out old buffers every hour
(add-to-list 'magic-mode-alist (cons "ELF" 'elf-mode))    ; call elf-mode for elf files

(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f10>"))                            ; was menu-bar-open
(global-unset-key (kbd "<f11>"))                            ; was fullscreen mode

;; frequently used functions
(use-package utilities
  :ensure nil
  :commands (me:set-extra-font-attributes
             me:select-nth-other-buffer
             me:select-2nd-other-buffer
             me:select-3rd-other-buffer
             me:select-4th-other-buffer
             me:select-5th-other-buffer
             me:find-some-files
             me:save-dirty-buffers )
  :config
  :load-path "lisp/")


;; infrequently used functions
(use-package extras
  :ensure nil
  :commands (me:find-other-file
             me:rotate-fill-column
             me:ps-one-per-page
             me:ps-two-per-page
             me:next-powerline-separator)
  :config
  :load-path "lisp/")


;; keymappings
(use-package general
  :config
  (general-evil-setup t)
  ;; Top level keymaps
  (general-define-key
    "<f2>"       #'ivy-switch-buffer
    "<f3>"       #'me:find-some-files
    "C-x b"      #'ivy-switch-buffer
    "C-h b"      #'counsel-descbinds
    "M-x"        #'counsel-M-x
    "s-1"        #'me:find-other-file
    "s-2"        #'me:select-2nd-other-buffer
    "s-3"        #'me:select-3rd-other-buffer
    "s-4"        #'me:select-4th-other-buffer
    "s-5"        #'me:select-5th-other-buffer
    "s-c"        #'me:rotate-fill-column
    "s-d"        #'company-complete
    "s-f"        #'flyspell-auto-correct-previous-word
    "s-S-f"      #'flyspell-correct-previous-word-generic
    "s-h"        #'git-gutter+-next-hunk
    "s-S-h"      #'git-gutter+-previous-hunk
    "s-s"        #'git-gutter+-stage-hunks
    "s-o"        #'git-gutter+-show-hunk-inline-at-point
    "s-]"        #'winner-redo
    "s-["        #'winner-undo
    "s-;"        #'rtags-location-stack-forward
    "s-,"        #'rtags-location-stack-back
    "s-`"        #'previous-buffer
    "s-\\"       #'me:next-powerline-separator
    "s-j"        (kbd "C-u 1 C-x `")
    "s-k"        (kbd "C-u -1 C-x `")
    "s-w"        #'ace-window
    "s-<right>"  #'persp-next
    "s-<left>"   #'persp-prev
    "<s-return>" #'yas-expand
    "<s-tab>"    #'next-buffer)

  (general-define-key
   :states '(normal visual emacs)
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
    "m"          #'projectile-compile-project
    "o"          #'me:switch-to-compile-buffer
    "r"          #'recompile
    "s"          #'me:use-evil-selection-register
    "v"          '(:ignore t :which-key "Multiple Cursors")
    "va"         #'evil-mc-make-all-cursors
    "vv"         #'evil-mc-make-and-goto-next-match
    "vs"         #'evil-mc-skip-and-goto-next-match
    "vx"         #'evil-mc-undo-all-cursors
    "v."         #'evil-mc-make-cursor-here
    "vp"         #'evil-mc-pause-cursors
    "vr"         #'evil-mc-resume-cursors
    "w"          #'save-buffer
    "x"          #'exchange-point-and-mark
    "<DEL>"      #'kill-this-buffer)

  ;; F4
  (general-define-key
   :prefix "<f4>"
    "a"     #'counsel-apropos
    "b"     #'counsel-mark-ring
    "d"     #'dired-jump
    "f"     #'counsel-git-grep
    "i"     #'counsel-info-lookup-symbol
    "j"     #'counsel-bookmark
    "k"     #'counsel-descbinds
    "l"     #'counsel-locate
    "n"     #'deft
    "m"     #'woman
    "p"     #'paradox-list-packages
    "r"     #'counsel-recentf
    "s"     #'counsel-ag
    "t"     #'shell-pop
    "u"     #'counsel-unicode-char
    "v"     #'undo-tree-visualize
    "y"     #'counsel-yank-pop
    "/"     #'treemacs-toggle
    "<f4>"  #'ivy-resume)

  ;; F5
  (general-define-key
   :prefix "<f5>"
    "t"      #'counsel-load-theme
    "f"      #'toggle-frame-fullscreen
    "h"      #'global-hl-line-mode
    "l"      #'linum-relative-mode
    "m"      #'menu-bar-mode
    "r"      #'ruler-mode
    "-"      #'text-scale-adjust
    "="      #'text-scale-adjust
    "<f5>"   #'menu-bar-open)

  ;; F6
  (general-define-key
   :prefix "<f6>"
    "c"     #'rtags-rename-symbol
    "d"     #'me:tags-find-symbol
    "f"     #'me:tags-find-file
    "i"     #'rtags-find-functions-called-by-this-function
    "m"     #'counsel-imenu
    "r"     #'me:tags-find-references-at-point
    "v"     #'rtags-find-virtuals-at-point
    "<RET>"  #'rtags-create-doxygen-comment
    "<f6>"  #'me:tags-find-symbol-at-point)

  ;; F7
  (general-define-key
   :prefix "<f7>"
    "f"     #'counsel-projectile-find-file
    "k"     #'projectile-kill-buffers
    "o"     #'projectile-multi-occur
    "r"     #'persp-rename
    "u"     #'projectile-invalidate-cache
    "/"     #'treemacs-projectile
    "<f7>"  #'projectile-persp-switch-project)

  ;; F8
  (general-define-key
   :prefix "<f8>"
   "v"      #'evil-window-split
   "h"      #'evil-window-vsplit
   "o"      #'delete-other-windows
   "x"      #'evil-window-delete
   "d"      #'evil-window-rotate-downwards
   "u"      #'evil-window-rotate-upwards
   "r"      #'windresize
   "1"      #'me:ps-one-per-page
   "2"      #'me:ps-two-per-page
   "<f8>"  #'delete-other-windows)

  ;; F9
  (general-define-key
   :prefix "<f9>"
    "b"     #'magit-blame
    "B"     #'magit-run-git-gui-blame
    "c"     #'magit-commit
    "a"     #'magit-commit-amend
    "i"     #'git-gutter+-show-hunk
    "l"     #'magit-log-current
    "<f9>"  #'magit-status)

  ;; Minibuffer keybindings
  (general-define-key
   :keymap minibuffer-local-map
   :prefix "C-c"
   "e" #'miniedit)

  :demand)

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

(use-package diminish
  :config
  :init
  ;; some of these need to be run after init file has loaded
  ;; to actually be diminished
  (add-hook 'after-init-hook (lambda ()
                               (diminish 'visual-line-mode)
                               (diminish 'abbrev-mode)
                               (diminish 'auto-revert-mode))))

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
    (add-hook 'prog-mode-hook #'fic-mode))

(use-package whitespace
  :commands whitespace-mode
  :config
  :init
    (setq whitespace-line-column nil                      ; highlight past fill-column
          whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab)
          whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
    (add-hook 'whitespace-mode-hook 'me:set-extra-font-attributes)
    (add-hook 'prog-mode-hook #'whitespace-mode)
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
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  :defer 4)

(use-package miniedit
  :commands miniedit
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
                (message "Updating %s" buffer);
                (kill-local-variable 'mode-line-format)
                (force-mode-line-update t))))
      (buffer-list)))

(use-package linum-relative
  :config
  :diminish linum-relative-mode
  :init
  (setq linum-relative-current-symbol ""))   ; show current line #

(use-package git-gutter-fringe+
  :if window-system
  :init
  :config
  (global-git-gutter+-mode t)
  :defer 1
  :diminish git-gutter+-mode)

(use-package ruler-mode
  :config)

;; better package manager
(use-package paradox
  :commands paradox-list-packages
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

(use-package ace-window
  :config
  :commands ace-window)

; modal window resizing
(use-package windresize
  :commands windresize
  :config
  :bind (:map windresize-map
              ("q" . windresize-exit)
              ("h" . windresize-left)
              ("l" . windresize-right)
              ("j" . windresize-down)
              ("k" . windresize-up)))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
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
  :commands dired-jump
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :config
    (use-package peep-dired
      :config
      :bind (:map dired-mode-map
              ("C-f" . peep-dired)))
    (use-package dired-narrow
      :config
      :bind (:map dired-mode-map
                  ("/" . dired-narrow)))
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
  :commands ( flyspell-prog-mode flyspell-mode flyspell-auto-correct-previous-word flyspell-correct-previous-word-generic)
  :init
  (setq ispell-personal-dictionary (expand-file-name "hunspell/words" me:config-directory))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'text-mode-hook #'flyspell-mode)
  :config
  (use-package flyspell-correct-ivy :demand)
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
  :diminish (flyspell-mode . "Ⓢ"))

(use-package ivy
  :bind
  (:map ivy-mode-map
        ("<escape>" . minibuffer-keyboard-quit)
        ("C-j" . ivy-next-line-and-call)
        ("C-k" . ivy-previous-line-and-call)
        ("C-=" . ivy-minibuffer-grow)
        ("C--" . ivy-minibuffer-shrink)
        ("C-'" . ivy-avy))
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

(use-package counsel
  :commands ( counsel-descbinds counsel-bookmark counsel-file-jump counsel-imenu counsel-yank-pop counsel-find-file)
  :init
  (setq counsel-yank-pop-separator "\n---\n")
  :config
  (counsel-mode 1)
  :diminish (counsel-mode . ""))

;; allow grep buffers to be editted
(use-package wgrep)

(use-package projectile
  :commands ( projectile-compile-project projectile-switch-project )
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
  (use-package persp-projectile :demand :config)
  (projectile-mode 1))

(use-package counsel-projectile
  :commands (counsel-projectile-find-file)
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
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config)

;; Highlight cursor's surrounding parentheses
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config)

(use-package markdown-mode
  :config
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package org
  :mode
  (("\\.org\\'" . org-mode))
  :config
    (use-package evil-org :config))

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
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


(use-package compile
  :commands compile
  :config
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
  :config)

(use-package woman
  :ensure nil
  :init
  (setq woman-use-topic-at-point t                          ; man page on word at point if exists
        Man-notify-method 'aggressive)                      ; show&select man page in other window
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

(use-package doc-view
  :ensure nil
  :config
  :init
  (setq doc-view-continuous t
        doc-view-resolution 144)
  :bind
  (:map doc-view-mode-map
        ("j" . doc-view-next-line-or-next-page)
        ("k" . doc-view-previous-line-or-previous-page)
        ("h" . image-backward-hscroll)
        ("l" . image-forward-hscroll)))

(use-package deft
  :config
  :init
  (setq deft-directory me:notes-path
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
  :commands ( global-company-mode company-complete )
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
  :commands ( rtags-location-stack-back rtags-location-stack-forward rtags-create-doxygen-comment)
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
  :config
  (rtags-diagnostics)
  (rtags-set-periodic-reparse-timeout 2)
  (rtags-enable-standard-keybindings))

;; yas snippets
(use-package yasnippet
  :commands ( yas-expand yas-expand-snippet )
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
  (add-hook 'prog-mode-hook #'flycheck-mode)
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
  (add-hook 'c-mode-common-hook   #'hs-minor-mode)
  (add-hook 'c++-mode-hook        #'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
  (add-hook 'sh-mode-hook         #'hs-minor-mode)
  (add-hook 'python-mode-hook     #'hs-minor-mode)
  :diminish (hs-minor-mode . "ⓕ"))

(use-package avy
  :commands ( avy-goto-word-1 avy-goto-char-2 avy-goto-char-in-line )
  :config
  :init
  (setq avy-all-windows 'all-frames))

;; N.B. evil-mode must be enabled after global-evil-leader-mode
(use-package evil
  :commands evil-mode
  :defer 3
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
  (defun me:switch-to-compile-buffer ()
    (interactive)
    (switch-to-buffer "*compilation*"))

  (use-package evil-args
    :commands (evil-inner-arg evil-outer-arg)
    :config )
  (use-package evil-textobj-anyblock
    :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block)
    :config )
  (use-package evil-commentary
    :commands (evil-commentary evil-commentary-yank)
    :config
    (evil-commentary-mode)
    :diminish evil-commentary-mode)
  (use-package evil-surround
    :commands (evil-surround-edit evil-Surround-edit evil-surround-region evil-Surround-region)
    :config (global-evil-surround-mode 1))
  (use-package evil-mc
    :commands (evil-mc-make-all-cursors
                evil-mc-make-cursor-here
                evil-mc-make-and-goto-next-match
                evil-mc-skip-and-goto-next-cursor
                evil-mc-make-cursor-here)
    :config
    (global-evil-mc-mode 1))
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'insert)
  (evil-set-initial-state 'magit-branch-manager-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-set-initial-state 'rtags-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'finder-mode 'emacs)
  (evil-set-initial-state 'doc-view-mode 'emacs)
  (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
  (evil-set-initial-state 'paradox-menu-mode 'emacs)

  ;; note evil-set-initial-state didn't work for these modes
  ;; I'm not sure why...
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (add-hook 'magit-log-mode-hook #'evil-emacs-state)
  (add-hook 'magit-revision-mode-hook #'evil-emacs-state)

  ;; Put view-mode in motion-state, this gives us motion
  ;; keys and not much more, this is good for read-only scenario
  ;; Since view-mode is combined with other modes, this needs
  ;; to be a hook.
  (add-hook 'view-mode-hook (lambda () (if view-mode (evil-motion-state) (evil-normal-state))))

  ;; extension key maps, mainly to autoload them
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg)
  (define-key evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
  (define-key evil-normal-state-map "gc" #'evil-commentary)
  (define-key evil-normal-state-map "gy" #'evil-commentary-yank)
  (define-key evil-operator-state-map "s" 'evil-surround-edit)
  (define-key evil-operator-state-map "S" 'evil-Surround-edit)
  (define-key evil-visual-state-map "S" 'evil-surround-region)
  (define-key evil-visual-state-map "gS" 'evil-Surround-region)

  (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
  ; esc key (from WikEmacs)
  (define-key evil-normal-state-map [escape] #'keyboard-quit)
  (define-key evil-visual-state-map [escape] #'keyboard-quit)
  ; scroll keeping cursor in place
  (define-key evil-normal-state-map (kbd "C-j")
    (lambda () (interactive)  (evil-scroll-line-down 1) (evil-next-visual-line 0)))
  (define-key evil-normal-state-map (kbd "C-k")
    (lambda () (interactive) (evil-scroll-line-up 1) (evil-previous-visual-line 0)))
  ; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") #'me:evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") #'me:evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] #'me:evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] #'me:evil-shift-left-visual)
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
  (which-key-setup-side-window-bottom)
  :defer 2
  :diminish which-key-mode)

(use-package magit
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-hook 'with-editor-mode-hook (lambda () (setq fill-column 70)))
  :config
  (use-package evil-magit
    :demand
    :init
    (setq evil-magit-state 'normal)))

(use-package treemacs
  :commands ( treemacs-toggle treemacs-projectile )
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

;; restore a normal gc threshold
(setq gc-cons-threshold 1000000)
