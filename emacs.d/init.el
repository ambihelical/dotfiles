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
(when (not package-archive-contents)
  (package-refresh-contents))
(when (not (package-installed-p 'use-package ))
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

;; set some personal variables
(defconst me:data-directory (if (getenv "XDG_DATA_HOME") (getenv "XDG_DATA_HOME") "~/.local/share"))
(defconst me:cache-directory (if (getenv "XDG_CACHE_HOME") (getenv "XDG_CACHE_HOME") "~/.cache"))
(defconst me:config-directory (if (getenv "XDG_CONFIG_HOME") (getenv "XDG_CONFIG_HOME") "~/.config"))
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

;; abort mini-buffer
(defun me:kill-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;; save any dirty buffers
(defun me:save-dirty-buffers ()
  "Save any dirty buffers"
  (interactive)
  (save-some-buffers t))

(defun me:find-some-files ()
  "Find files in project or fallback to current directory"
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-find-file)
    (counsel-find-file)))

;; configuration I haven't figured out how to wedge into
;; use-package

(setq-default tab-width 3                                   ; ideal tab width
              indent-tabs-mode t                            ; enable tabs for most files
              indicate-empty-lines t                        ; show empty lines at end of buffer
              fill-column 120)                              ; auto-wrap only very long lines
(setq backup-directory-alist
         `((".*" . ,me:emacs-backup-directory))             ; use my backup directory
      inhibit-splash-screen t                               ; no splash
      inhibit-startup-echo-area-message t                   ; no startup message
      inhibit-startup-message t                             ; no startup message
      frame-title-format
        '((:eval (if (buffer-file-name)
                    (me:replace-prefix (abbreviate-file-name default-directory)
                                        (abbreviate-file-name buffer-file-name))
                  "%b"))
          " %* ["
          (:eval (abbreviate-file-name default-directory))
          "]")                                              ; fancy title
      icon-title-format frame-title-format                  ; use same title for unselected frame
      mouse-wheel-scroll-amount '(3 ((shift) . 9))          ; 3 lines, or 9 line when shift held
      mouse-wheel-follow-mouse 't                           ; scroll window under mouse
      mouse-wheel-progressive-speed nil                     ; don't speed up
      custom-file "/dev/null"                               ; disable customizations
      initial-scratch-message nil                           ; no scratch message
      initial-major-mode 'text-mode                         ; no prog-mode at startup
      kill-ring-max 200                                     ; More killed items
      kill-do-not-save-duplicates t                         ; No duplicates in kill ring
      history-length 1000                                   ; length of history
      save-interprogram-paste-before-kill t                 ; save clipboard before killing
      select-enable-clipboard nil                           ; make cut/paste function correctly
      x-gtk-use-system-tooltips nil                         ; allow tooltip theming
      load-prefer-newer t                                   ; load source if newer than bytecode
      split-width-threshold 240                             ; 2x ideal line width :)
      visual-line-fringe-indicators '(left-curly-arrow nil) ; use left curly error for wrapped lines
      text-scale-mode-step 1.05                             ; text size increases by 5% (normally 20%)
      view-read-only t                                      ; show r/o files in view mode
      enable-recursive-minibuffers t                        ; allow recursive edit
      standard-indent 3                                     ; ideal indent :)
      scroll-margin 5                                       ; show some lines around cursor when possible
      fast-but-imprecise-scrolling t                        ; quick and dirty scrolling
      sentence-end-double-space nil)                        ; sentences end with one space
(add-hook 'focus-out-hook #'me:save-dirty-buffers)          ; save on defocus
(add-hook 'mouse-leave-buffer-hook #'me:kill-minibuffer)    ; kill minibuffer on click outside
(add-hook 'after-init-hook                                  ; report init time
          (lambda ()
            (message "Time to initialize: %s"
                     (emacs-init-time))))
(add-hook 'prog-mode-hook                                   ; stuff for all programming modes
          (lambda ()
             (modify-syntax-entry ?_ "w")))                 ; underscores are parts of words
(set-frame-font "Fantasque Sans Mono 12" t t)               ; nice font
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

;; some demand loaded commands I wrote
(use-package me:extras
  :ensure nil
  :commands (me:select-nth-other-buffer me:rotate-fill-column me:next-powerline-separator)
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
    "s-1"        #'projectile-find-other-file
    "s-#"        #'me:select-nth-other-buffer    ; base mapping for the following . ..
    "s-2"        (kbd "C-u 1 s-#")
    "s-3"        (kbd "C-u 2 s-#")
    "s-4"        (kbd "C-u 3 s-#")
    "s-5"        (kbd "C-u 4 s-#")
    "s-c"        #'me:rotate-fill-column
    "s-d"        #'company-complete
    "s-f"        #'flyspell-auto-correct-previous-word
    "s-s"        #'flyspell-correct-previous-word-generic
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
    "v"          #'exchange-point-and-mark
    "w"          #'save-buffer
    "<DEL>"      #'kill-this-buffer)

  ;; F4
  (general-define-key
   :prefix "<f4>"
    "a"     #'helm-apropos
    "b"     #'helm-all-mark-rings
    "d"     #'dired-jump
    "f"     #'counsel-git-grep
    "i"     #'counsel-info-lookup-symbol
    "j"     #'counsel-bookmark
    "k"     #'counsel-descbinds
    "l"     #'counsel-locate
    "n"     #'deft
    "m"     #'helm-man-woman
    "p"     #'paradox-list-packages
    "r"     #'counsel-recentf
    "s"     #'counsel-ag
    "t"     #'shell-pop
    "u"     #'counsel-unicode-char
    "v"     #'undo-tree-visualize
    "x"     #'helm-top
    "y"     #'helm-show-kill-ring
    "<f4>"  #'ivy-resume)

  ;; F5
  (general-define-key
   :prefix "<f5>"
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
    "a"     #'magit-run-git-gui-blame
    "b"     #'magit-blame
    "f"     #'counsel-projectile-find-file
    "g"     #'magit-status
    "k"     #'projectile-kill-buffers
    "o"     #'projectile-multi-occur
    "r"     #'persp-rename
    "u"     #'projectile-invalidate-cache
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
   "<f8>"  #'delete-other-windows)
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
  (progn
    (setq fic-highlighted-words `( "TODO" "HACK" "KLUDGE" "FIXME" "TRICKY" "BUG" ))
    (add-hook 'prog-mode-hook #'fic-mode)))

(use-package whitespace
  :commands whitespace-mode
  :config
  :init
  (progn
    (setq whitespace-line-column nil                      ; highlight past fill-column
          whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab)
          whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
    (add-hook 'prog-mode-hook #'whitespace-mode))
  :diminish whitespace-mode)

;; N.B. Disabled because it seems to interfere with popups
(use-package fill-column-indicator
  :disabled
  :config
  (progn
    (add-hook 'after-change-major-mode-hook (lambda () (if buffer-file-name (fci-mode 1))))
    (setq fci-rule-color "white smoke")))

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
  (progn
    (setq-default adaptive-wrap-extra-indent 3)
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))
  :defer 4)

(use-package leuven-theme
  :defer 0
  :init
  (add-hook 'whitespace-mode-hook (lambda ()
    (set-face-attribute 'whitespace-line nil :foreground 'unspecified :background "lemon chiffon")
    (set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background "white" )
    (set-face-attribute 'whitespace-trailing nil :foreground "black" :background "red" )))
  :config
  (set-face-attribute 'hl-line nil :foreground 'unspecified :background "gainsboro"))


(use-package spaceline-config
  :if window-system
  :ensure spaceline
  :defer 2
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator " ")
  :config
  (progn
    (use-package powerline
      :config
      :demand
      :init
      (setq powerline-default-separator 'wave))
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
        (buffer-list))))

;; show change indicators in fringe
(use-package diff-hl
  :if window-system
  :init
  (progn
    (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  :config
  (progn
    (when (not (display-graphic-p))
      (diff-hl-margin-mode 1))
    (diff-hl-flydiff-mode 1)       ; needs 24.4 or newer
    (global-diff-hl-mode 1))
  :defer 4)


(use-package linum-relative
  :config
  :diminish linum-relative-mode
  :init
  (setq linum-relative-current-symbol ""))   ; show current line #

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

(use-package dired
  :commands dired-jump
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :config
  (progn
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
            diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)))
  :ensure nil)

(use-package flyspell
  :commands ( flyspell-prog-mode flyspell-mode flyspell-auto-correct-previous-word flyspell-correct-previous-word-generic)
  :init
  (progn
    (setq ispell-personal-dictionary (expand-file-name "hunspell/words" me:config-directory))
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (add-hook 'text-mode-hook #'flyspell-mode))
  :config
  (progn
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
        (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))))
  :diminish (flyspell-mode . "Ⓢ"))

(use-package ivy
  :bind
  (:map ivy-mode-map
        ("<escape>" . minibuffer-keyboard-quit)
        ("C-j" . ivy-next-line-and-call)
        ("C-k" . ivy-previous-line-and-call)
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  :init
  (setq ivy-use-virtual-buffers t                           ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-height 15                                       ; number of result lines to display
        ivy-count-format ""                                 ; does not count candidates
        ivy-initial-inputs-alist nil                        ; no regexp by default
        ivy-re-builders-alist
           '((t . ivy--regex-ignore-order)))                ; allow input not in order
  :diminish (ivy-mode . ""))

(use-package counsel
  :commands ( counsel-descbinds counsel-bookmark counsel-file-jump counsel-imenu )
  :config
  (counsel-mode 1)
  :diminish (counsel-mode . ""))

;; allow grep buffers to be editted
(use-package wgrep)

(use-package helm
  :commands ( helm-top helm-man-woman helm-top helm-all-mark-rings)
  :init
  (setq helm-split-window-in-side-p           t
        helm-split-window-default-side        'other
        helm-move-to-line-cycle-in-source     t
        helm-input-idle-delay                 0.1
        helm-candidate-number-limit           100
        helm-scroll-amount                    8
        helm-buffer-max-length                40           ; for displayed file names
        helm-autoresize-max-height            33           ; 33% of frame (requires autoresize mode)
        helm-autoresize-min-height            33           ; 33% of frame (requires autoresize mode)
        helm-follow-mode-persistent           t            ; follow-mode persists across sessions
        helm-ff-skip-boring-files             t)            ; don't show boring files
  :config
  (progn
    (helm-autoresize-mode t)
    ;; swap tab/c-z as recommended by tuhdo
    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action) ; rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  #'helm-select-action) ; list actions using C-z
    (define-key helm-map [escape] #'helm-keyboard-quit)))

(use-package projectile
  :commands ( projectile-compile-project projectile-switch-project )
  :diminish projectile-mode
  :after evil
  :init
  (progn
    (setq projectile-completion-system 'ivy
          projectile-globally-ignored-files #'( "TAGS" "GTAGS" "GRTAGS" "GPATH" )
          projectile-globally-ignored-file-suffixes #'( ".o" ".so" ".a" ".ko" ".jar" ".bc")
          projectile-use-git-grep t
          projectile-project-root-files-functions
            #'( projectile-root-local projectile-root-top-down projectile-root-bottom-up projectile-project-root-files-top-down-recurring)
          projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))
          projectile-enable-caching t))
  :config
  (progn
    (push "compile_commands.json" projectile-project-root-files)
    (push "build" projectile-globally-ignored-directories)
    (use-package counsel-projectile
      :config
      (counsel-projectile-on))
    (use-package persp-projectile :demand :config)
    (projectile-global-mode 1)))

(use-package perspective
  :after projectile
  :config
  (progn
    (setq persp-initial-frame-name (projectile-project-name))
    (persp-mode)))

(use-package smart-tabs-mode
  :commands (smart-tabs-mode)
  :init
  (progn
    (add-hook 'c-mode-common-hook #'smart-tabs-mode)
    (add-hook 'python-mode-hook #'smart-tabs-mode))
  :config
  (progn
    (smart-tabs-insinuate 'python)
    (smart-tabs-insinuate 'c 'c++)))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :config
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package markdown-mode
  :config
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package org
  :mode
  (("\\.org\\'" . org-mode))
  :config
  (progn
    (use-package evil-org :config)))

(use-package cmake-mode
  :config
  :init
  (progn
    (setq cmake-tab-width 3))
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

;; n.b. buffer-face-mode screws up completion popups
;; may be fixed in 25.1 or so
(use-package adoc-mode
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case)
  :config
  :init
  (progn
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
    (add-hook 'adoc-mode-hook
              (lambda ()
                (setq company-dabbrev-downcase nil     ; don't downcase completions
                      company-dabbrev-ignore-case nil  ; don't keep prefix
                      evil-shift-width 4               ; set tabs 4 spaces
                      tab-width 4
                      indent-tabs-mode nil))))
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

(use-package cc-mode
  :init
  (progn
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
          show-paren-mode 0))

  :config
  :mode
  (("\\.c\\'"   . c-mode)
   ("\\.cpp\\'" . c++-mode)
   ("\\.cxx\\'" . c++-mode)
   ("\\.h\\'"   . c++-mode)
   ("\\.hpp\\'" . c++-mode)))

(use-package compile
  :commands compile
  :config
  :init
  (progn
    (defun me:rotate-skip-threshold ()
      (interactive)
      (compilation-set-skip-threshold
       (cond ((= compilation-skip-threshold 1) 2)
             ((= compilation-skip-threshold 2) 0)
             (t 1))))
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
        (define-key compilation-mode-map (kbd "<tab>") #'me:rotate-skip-threshold)
        (define-key compilation-mode-map (kbd "f") #'next-error-follow-minor-mode)
        (define-key compilation-mode-map (kbd "k") #'compilation-previous-error)
        (define-key compilation-mode-map (kbd "j") #'compilation-next-error)
        (define-key compilation-mode-map (kbd "C-k") #'compilation-previous-file)
        (define-key compilation-mode-map (kbd "C-j") #'compilation-next-file)
        (define-key compilation-mode-map (kbd "<prior>") #'compilation-previous-error)
        (define-key compilation-mode-map (kbd "<next>") #'compilation-next-error)
        (define-key compilation-mode-map (kbd "<home>") #'compilation-previous-file)
        (define-key compilation-mode-map (kbd "<end>") #'compilation-next-file)))))

;; view symbols of libraries
(use-package elf-mode
  :config)

(use-package doc-view
  :ensure nil
  :config
  :init
  (progn
    (setq doc-view-continuous t
          doc-view-resolution 144))
  :bind
  (:map doc-view-mode-map
        ("j" . doc-view-next-line-or-next-page)
        ("k" . doc-view-previous-line-or-previous-page)
        ("h" . image-backward-hscroll)
        ("l" . image-forward-hscroll)))

(use-package deft
  :config
  :init
  (progn
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
                (define-key deft-mode-map (kbd "<C-backspace>") #'deft-filter-clear)))))

(use-package company
  :commands ( global-company-mode company-complete )
  :init
  (setq company-minimum-prefix-length 1)
  :config
  (progn
    (use-package company-quickhelp
      :demand
      :config
      (company-quickhelp-mode 1))
    (global-company-mode))
  :diminish company-mode)

(use-package helm-gtags
  :commands helm-gtags-mode
  :init
  (progn
    (setq helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-ignore-case t)
    (add-hook 'dired-mode-hook #'helm-gtags-mode)
    (add-hook 'eshell-mode-hook #'helm-gtags-mode)
    (add-hook 'c-mode-hook #'helm-gtags-mode)
    (add-hook 'c++-mode-hook #'helm-gtags-mode)
    (add-hook 'asm-mode-hook #'helm-gtags-mode))
  :config
  (progn
    (defun me:update-all-tags ()
      (interactive)
      (let ((current-prefix-arg 4)) (call-interactively #'helm-gtags-update-tags))))
  :diminish helm-gtags-mode)

(use-package rtags
  :commands ( rtags-location-stack-back rtags-location-stack-forward rtags-create-doxygen-comment)
  :init
  (progn
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
          (cond ((and (not (eq major-mode 'c++-mode))
                      (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
                (useFileManager (rtags-has-filemanager))
                (t (rtags-is-indexed)))))
    (defun me:tags-find-symbol-at-point (&optional prefix)
      (interactive "P")
      (if (not (rtags-find-symbol-at-point prefix))
          (call-interactively #'helm-gtags-dwim)))
    (defun me:tags-find-references-at-point (&optional prefix)
      (interactive "P")
      (if (not (rtags-find-references-at-point prefix))
          (call-interactively #'helm-gtags-find-rtag)))
    (defun me:tags-find-symbol ()
      (interactive)
      (call-interactively (if (me:use-rtags) #'rtags-find-symbol #'helm-gtags-find-symbol)))
    (defun me:tags-find-file ()
      (interactive)
      (call-interactively (if (me:use-rtags t) #'rtags-find-file #'helm-gtags-find-files)))
    (setq rtags-autostart-diagnostics t
          rtags-use-helm t
          rtags-tooltips-enabled nil
          rtags-display-current-error-as-message nil
          rtags-process-flags "--config ~/.config/rtags/config"
          rtags-completions-enabled t)
    (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
    (add-hook 'c-mode-common-hook #'me:flycheck-rtags-setup)
    (add-hook 'company-mode-hook  #'me:company-rtags-setup))
  :config
  (progn
    (rtags-diagnostics)
    (rtags-set-periodic-reparse-timeout 2)
    (rtags-enable-standard-keybindings)))

;; yas snippets
(use-package yasnippet
  :commands ( yas-expand yas-expand-snippet )
  :init
  (progn
    (setq yas-fallback-behavior 'return-nil)          ; don't try old binding
    (add-hook 'yas-before-expand-snippet-hook         ; evil-insert at each slot
              #'(lambda()
                    (let ((p (point)) (m (mark)))
                      (evil-insert-state)
                      (goto-char p)
                      (set-mark m)))))
  :config
  (progn
    (define-key yas-minor-mode-map (kbd "<tab>") nil) ; don't use <tab>
    (define-key yas-minor-mode-map (kbd "TAB") nil)   ; don't use TAB
    (yas-global-mode))
  :diminish (yas-minor-mode . "Ⓨ"))

(use-package flycheck
  :commands flycheck-mode
  :diminish ( flycheck-mode . "ⓒ")
  :init
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (add-hook 'prog-mode-hook #'flycheck-mode))
  :config
  (progn
    (use-package flycheck-pos-tip
      :demand
      :init
      (progn
        (setq flycheck-pos-tip-timeout 3))
      :config
      (progn
        (flycheck-pos-tip-mode t)))
    (set-face-attribute 'flycheck-warning nil :foreground 'unspecified :background "khaki1")
    (set-face-attribute 'flycheck-error nil :foreground 'unspecified :background "light pink")))

;; enable code folding (evil has bindings)
(use-package hideshow
  :commands hs-minor-mode
  :config
  :init
  (progn
    (add-hook 'c-mode-common-hook   #'hs-minor-mode)
    (add-hook 'c++-mode-hook        #'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
    (add-hook 'sh-mode-hook         #'hs-minor-mode)
    (add-hook 'python-mode-hook     #'hs-minor-mode))
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
  (progn
    (add-hook 'prog-mode-hook #'evil-mode)
    (add-hook 'text-mode-hook #'evil-mode)
    (setq-default evil-symbol-word-search t   ; misnamed: t is search for symbols, not words
                  evil-shift-width 3)         ; shift by ideal width :)
    (setq evil-want-C-w-delete nil            ; want C-w it for windows commands
          evil-want-C-w-in-emacs-state t      ; ditto
          evil-want-C-i-jump nil              ; need TAB for other things
          evil-search-module #'evil-search))
  :config
  (progn
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
    (evil-mode 1)))

(use-package ws-butler
  :config
  (progn
    ;; work around current tab replacement behavior
    (defun ws-butler-clean-region (beg end)
      "Delete trailing blanks in region BEG END."
      (interactive "*r")
      (ws-butler-with-save
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1)))
      nil)
    (ws-butler-global-mode t))
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
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom))
  :defer 2
  :diminish which-key-mode)

(use-package magit
  :init
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    (add-hook 'with-editor-mode-hook (lambda () (setq fill-column 70))))
  :config
  (progn
    (use-package evil-magit
      :demand
      :init
      (setq evil-magit-state 'normal))))

;; N.B. disabling this because once neotree is popped up once, tab completion in mini-buffer
;; is screwed up.
(use-package neotree
  :disabled
  :init
  (progn
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t)))
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") #'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") #'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "q") #'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") #'neotree-enter))))
  :bind
  (("<f4> /" . neotree-toggle)
   ("<f7> /" . neotree-projectile-action)))

(use-package popwin
  :init
  (progn
    (setq display-buffer-function #'popwin:display-buffer))
  :config
  (progn
    (push '("*Async Shell Command*" :noselect t) popwin:special-display-config)
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
    (push '("*Help*" :stick t ) popwin:special-display-config)
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)
    (push '("*undo-tree*" :stick t :width 60 :position right) popwin:special-display-config)
    (popwin-mode 1))
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
