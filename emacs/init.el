;;; -*- lexical-binding: t -*-

;; this is set in early-init and here in case emacs < 27
;; the package gcmh sets it to a reasonable value later
(setq gc-cons-threshold most-positive-fixnum)

;; use-package
(unless (package-installed-p 'use-package )
  (package-install 'use-package))

(eval-when-compile
  (require 'subr-x)                                               ; need string functions
  (setq use-package-enable-imenu-support t)                       ; support for packages in imenu
  (require 'use-package))

(eval-after-load "use-package"
  '(setq use-package-always-ensure t                              ; ensure by default
         use-package-always-defer t                               ; defer by default
         use-package-minimum-reported-time 0.03                   ; minimum time when verbose
         use-package-verbose nil))                                ; don't be verbose

;; xdg directories
(defconst me:data-directory (or (getenv "XDG_DATA_HOME") (expand-file-name ".local/share" "~")))
(defconst me:cache-directory (or (getenv "XDG_CACHE_HOME") (expand-file-name ".cache" "~")))
(defconst me:config-directory (or (getenv "XDG_CONFIG_HOME")  (expand-file-name ".config" "~")))

(defconst me:default-font
  (pcase (system-name)
    ("hum" "Roboto Mono-11:autohint=true")
    ("thud" "Roboto Mono-12:autohint=true")
    ("SG267" "Roboto Mono-12:autohint=true")
    ("SG296" "Roboto Mono-12:autohint=true")
    (_ "DejaVu Sans Mono-12")))
(add-to-list 'default-frame-alist `(font . ,me:default-font))
(add-to-list 'default-frame-alist `(fullscreen . maximized))

;; Setup theme load hook
(defvar me:after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun me:run-after-load-theme-hook (&rest _)
  "Run `me:after-load-theme-hook'."
  (run-hooks 'me:after-load-theme-hook))
(advice-add #'load-theme :after #'me:run-after-load-theme-hook)

;; N.B. some init code depends on this being set correctly
(defvar me:theme-is-dark-p nil
  "true if theme is dark")

;; Run programming mode hooks
;; This is used for modes which should trigger programming mode hooks
(defun me:run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

;; Disable customize, borrowed from doom emacs
(dolist (sym '(customize-option customize-browse customize-group customize-face
                                customize-rogue customize-saved customize-apropos
                                customize-changed customize-unsaved customize-variable
                                customize-set-value customize-customized customize-set-variable
                                customize-apropos-faces customize-save-variable
                                customize-apropos-groups customize-apropos-options
                                customize-changed-options customize-save-customized))
  (put sym 'disabled "`customize' unsupported, configure Emacs from init.el instead"))
(put 'customize-themes 'disabled "Use `load-theme' in init.el instead")

;; Emacs configuration
(use-package emacs
  :custom
  (help-enable-symbol-autoload t)                         ; autoload symbol for showing help
  (imenu-max-item-length 200)                             ; default of 60 too short for some c++ methods
  (ad-redefinition-action 'accept)                        ; turn off 'xyz' got redefined warnings
  (confirm-kill-processes nil)                            ; don't ask about killing processes at exit
  (create-lockfiles nil)                                  ; no lockfiles (.#file)
  (debugger-stack-frame-as-list t)                        ; show fns as (fn args) instead of fn(args)
  (describe-bindings-outline t)                           ; use outlines for describe bindings C-h b
  (fast-but-imprecise-scrolling t)                        ; quick and dirty scrolling
  (find-file-visit-truename t)                            ; resolve symlinks finding files
  (history-length 1000)                                   ; length of history
  (history-delete-duplicates t)                           ; don't allow repeated history
  (inhibit-splash-screen t)                               ; no splash
  (inhibit-startup-echo-area-message t)                   ; no startup message
  (inhibit-startup-message t)                             ; no startup message
  (initial-major-mode 'text-mode)                         ; no prog-mode at startup
  (initial-scratch-message nil)                           ; no scratch message
  (mouse-wheel-scroll-amount '(3 ((shift) . 9)))          ; 3 lines, or 9 line when shift held (mwheel)
  (mouse-wheel-follow-mouse 't)                          ; scroll window under mouse (mwheel)
  (mouse-wheel-progressive-speed nil)                     ; don't speed up (mwheel)
  (next-error-message-highlight 'keep)                    ; highlight visited errors
  (ring-bell-function 'ignore)                            ; don't ring bell
  (undo-limit 1000000)                                    ; 1M (default is 80K)
  (undo-strong-limit 1500000)                             ; 1.5M (default is 120K)
  (undo-outer-limit 150000000)                            ; 150M (default is 12M)
  (use-file-dialog nil)                                   ; never want gui file dialog
  (use-dialog-box nil)                                    ; never want dialog box for questions
  (use-short-answers t)                                   ; use y/n
  (save-some-buffers-default-predicate 'save-some-buffers-root)  ; same only project files
  (scroll-margin 5)                                       ; show some lines around cursor when possible
  (scroll-conservatively 101)                             ; only scroll enough to bring cursor to view
  (scroll-down-aggressively 0.01)                         ; don't jump when scrolling down
  (scroll-up-aggressively 0.01)                           ; don't jump when scrolling up
  (scalable-fonts-allowed t)                              ; allow any scalable font
  (select-enable-clipboard nil)                           ; make cut/paste function correctly (select)
  (sentence-end-double-space nil)                         ; sentences end with one space
  (x-gtk-use-system-tooltips nil)                        ; allow tooltip theming

  :init
  (setq-default fill-column 80                                ; auto-wrap only very long lines
                bidi-paragraph-direction 'left-to-right       ; onedi for me
                tab-width 4                                   ; default tab width
                minibuffer-follows-selected-frame nil         ; minibuffer stays in frame
                indicate-empty-lines t)                       ; show empty lines at end of buffer
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)  ; customize file
        bidi-inhibit-bpa t                                    ; disable bpa
        read-process-output-max (* 1024 64)                   ; read more in one go from subprocesses
        load-prefer-newer t)                                  ; load source if newer than bytecode
  (when (eq window-system 'w32)
    ;; This is slower but allows dired-subtree to detect directories correctly
    ;; ls-lisp seems to have a funky line format which dired-subtree doesn't understand
    (setq ls-lisp-use-insert-directory-program t))
  (run-with-idle-timer 0.1 nil #'me:deferred-setup)

  :hook (after-init . me:after-init)

  :config
  (defun me:after-init ()
    (message "Time to initialize: %s"
             (emacs-init-time))
    ;; remove extraneous quickstart files found after clean install
    (delete-file (expand-file-name "package-quickstart.el" user-emacs-directory))
    (delete-file (expand-file-name "package-quickstart.elc" user-emacs-directory)))
  (defun me:deferred-setup ()
    (tool-bar-mode 0)                                           ; no tool bar (tool-bar)
    (scroll-bar-mode 0)                                         ; no scroll bar (scroll-bar)
    (mouse-avoidance-mode 'animate)                             ; move mouse pointer out of way (avoid)
    (electric-indent-mode +1)                                   ; turn on electric mode globally (electric)
    (delete-selection-mode t)                                   ; pastes delete selection
    (blink-cursor-mode -1)                                      ; don't blink cursor
    (global-so-long-mode +1)                                    ; handle long lines better
    ;; add mingw64 paths under windows
    (when (eq window-system 'w32)
      (add-to-list 'exec-path "c:/Program Files/Git/mingw64/bin/")
      (add-to-list 'exec-path "c:/Program Files/Git/bin/")
      (add-to-list 'exec-path "c:/Program Files/Git/usr/bin/")
      (add-to-list 'exec-path (expand-file-name "bin/" "~")))
    (unless (display-graphic-p)
      ;; use mouse in xterm mode
      (xterm-mouse-mode t)
      ;; handle xterm special sequences
      (define-key input-decode-map "\e[46;5u" (kbd "C-."))
      (define-key input-decode-map "\e[44;5u" (kbd "C-,"))
      (define-key input-decode-map "\e[60;6u" (kbd "C-<"))
      (define-key input-decode-map "\e[62;6u" (kbd "C->"))
      (define-key input-decode-map "\e[59;5u" (kbd "C-;"))
      (define-key input-decode-map "\e[40;6u" (kbd "C-("))
      (define-key input-decode-map "\e[41;6u" (kbd "C-)"))
      (define-key input-decode-map "\e[49;5u" (kbd "C-1"))
      (define-key input-decode-map "\e[39;5u" (kbd "C-'"))
      (define-key input-decode-map "\e[45;5u" (kbd "C--"))
      (define-key input-decode-map "\e[43;6u" (kbd "C-+"))
      (define-key input-decode-map "\e[61;5u" (kbd "C-="))
      (define-key input-decode-map "\e[63;6u" (kbd "C-?")))))

;; garbage collection magic hack
(use-package gcmh
  :custom
  (gcmh-verbose t)
  ;; otherwise get GC times in 10s of seconds
  (gcmh-high-cons-threshold #x10000000)
  :config
  (gcmh-mode 1)
  :demand)

;; keymappings
;; N.B. Other keybindings defined in apropriate use-package
(use-package general
  :init
  (global-unset-key (kbd "<f4>"))
  (global-unset-key (kbd "<f10>"))
  :config
  (general-evil-setup t)
  (general-override-mode)
  (general-define-key :prefix "<f4>" "K"     #'general-describe-keybindings)
  :demand)

;; make describe-xxx more useful
(use-package helpful
  :commands ( helpful-callable helpful-key helpful-function helpful-callable helpful-variable helpful-symbol )
  :config
  (setq elisp-refs-verbose nil)
  :general
  ([remap describe-key] #'helpful-key)
  ([remap describe-function] #'helpful-callable)
  ([remap describe-variable] #'helpful-variable)
  ([remap describe-symbol] #'helpful-symbol)
  :config)

(use-package elisp-demos
  :after helpful
  :demand
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package hydra
  :commands (defhydra)
  :config
  :init
  (setq hydra-base-map (make-sparse-keymap)))

;; infrequently used functions
(use-package extras
  :commands (x-urgency-hint)
  :ensure nil
  :general
  ("<f10> s"  #'me:read-fill-column)
  ("<f4> 1"  #'me:ps-one-per-page)
  ("<f4> 2"  #'me:ps-two-per-page)
  :config
  :load-path "lisp/")

;; built-in emacs-lisp-mode package
(use-package emacs-lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . me:emacs-lisp-config)
  :general
  (:keymaps 'emacs-lisp-mode-map :prefix "C-c"
            "v"          #'pp-eval-last-sexp
            "x"          #'pp-macroexpand-last-sexp)
  :init
  (defun me:emacs-lisp-config ()
    (when (and (bobp) (eobp))                     ; put lexical binding in empty files
      (setq lexical-binding t)
      (insert ";;; -*- lexical-binding: t; -*-\n\n")))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package highlight-function-calls
  :init
  :config
  ;; more subtle highlighting for functions
  (set-face-attribute 'highlight-function-calls-face nil :inherit font-lock-type-face :underline nil :italic t )
  :hook (emacs-lisp-mode . highlight-function-calls-mode))

;; built-in frame package
;; Because there is no window package, window config is here as well
(use-package frame
  :ensure nil
  :custom
  (split-width-threshold 200)                ; don't split unless have 100 columns for each
  (split-height-threshold 120)               ; almost never split vertically
  :general
  ("<f10> f"     #'toggle-frame-fullscreen)   ; frame
  ("M-`"        #'previous-buffer)           ; window
  ("M-~"        #'next-buffer)               ; window
  ("M-w"        #'other-window)              ; window
  :init
  (add-function :after after-focus-change-function
                (lambda () (unless (frame-focus-state)
                             (save-some-buffers t))))

  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (string-remove-prefix (abbreviate-file-name default-directory)
                                           (abbreviate-file-name buffer-file-name))
                   "%b"))
          " %* ["
          (:eval (abbreviate-file-name default-directory))
          "]")                                               ; fancy title
        icon-title-format frame-title-format)                  ; use same title for unselected frame
  :config

  (defun me:window-nth-buffer (arg &optional prefix)
    "Select the nth other buffer. Use prefix to put in other window"
    (interactive "P")
    (when-let* ((bufs (mapcar 'car (window-prev-buffers)))
                (buffer (nth arg (remove (current-buffer) bufs))))
      (if prefix
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))))

  ;; Define M-2 to M-9 as selecting the nth buffer
  (dotimes (ind 8) ;; 0 to 7
    (let ((key (+ 2 ind)))
      (global-set-key (kbd (concat "M-" (format "%d" key)))
                      (lambda (&optional prefix)
                        (interactive "P")
                        (me:window-nth-buffer ind prefix)))))

  :demand)

;; built-in winner package
;; N.B. winner-mode must be started early to record window configs
(use-package winner
  :ensure nil
  :general
  ("C-}"   #'winner-redo)
  ("C-{"   #'winner-undo)
  :defer 1
  :init
  (setq winner-dont-bind-my-keys t)    ;; don't bind C-c left/right,etc
  :config
  (winner-mode t))

;; builtin midnight package
(use-package midnight
  :disabled
  :ensure nil
  :defer 1
  :custom
  (clean-buffer-list-delay-general 2)
  :config
  (setq clean-buffer-list-kill-never-regexps
        (nconc clean-buffer-list-kill-never-regexps
               '("\\`\\*tramp/.*\\*\\`"
                 "\\`\\magit.*\\`"
                 "\\`\\*ftp .*\\*\\`")))
  (setq clean-buffer-list-kill-never-buffer-names
        (nconc clean-buffer-list-kill-never-buffer-names
               '( "*git-credential-cache--daemon*" )))
  (run-at-time "1 hour" 3600 #'clean-buffer-list)             ; clear out old buffers every hour (midnight)
  (midnight-mode t))

;; built-in minibuffer package
(use-package minibuffer
  :ensure nil
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (enable-recursive-minibuffers t)                        ; allow recursive edit
  :config
  (minibuffer-depth-indicate-mode t)       ; show recursive edit depth (mb-depth)
  :defer 0.5)

;; built-in autorevert package
(use-package autorevert
  :hook (( prog-mode text-mode ) . auto-revert-mode )
  :ensure nil
  :defer 2
  :custom
  ;; N.B. auto-revert-check-vc-info causes vc-refresh-state to be called
  ;; on *every* file-backed buffer every 5s by default even if it isn't reverted.
  ;; This causes vc-mode-line to be called for each such buffer.
  ;; For this reason, never set this variable.
  (auto-revert-check-vc-info nil)             ; don't check periodically for new vc info
  (auto-revert-verbose nil))                  ; don't tell me about auto reverts

;; built-in "simple" package
(use-package simple
  :hook (( prog-mode text-mode ) . visual-line-mode )
  :ensure nil
  :custom
  (kill-do-not-save-duplicates t)          ; No duplicates in kill ring
  (kill-ring-max 200)                      ; more killed items
  (visual-line-fringe-indicators '(left-curly-arrow nil))  ; use left curly error for wrapped lines
  :general
  ("M-n"        #'next-error)
  ("M-p"        #'previous-error)
  ("C-M-<backspace>"    #'kill-current-buffer)
  :config
  (column-number-mode t)                       ; display column/row of cursor in mode-line
  :defer 1)

;; built-in "abbrev" package
(use-package abbrev
  :ensure nil
  :general
  (:prefix "C-x"
           "a"  '(:ignore t :which-key "Abbrev→" ))
  :defer 1)

;; built-in "artist-mode" package
;; No binding because this one will be rare
(use-package artist-mode
  :hook (artist-mode . me:artist-mode-config)
  :ensure nil
  :init
  (defun me:artist-mode-config ()
    (if artist-mode
        ( evil-emacs-state t)
      (evil-normal-state t)))
  :general
  (:prefix "C-c"
           "C-a"  '(:ignore t :which-key "Artist→" )))

;; built-in "hl-line" package
(use-package hl-line
  :ensure nil
  :general
  ("<f10> h" #'global-hl-line-mode)              ; toggle hl-line
  :config
  (global-hl-line-mode t)                       ; highlight current line (hl-line)
  :defer 1)

;; built-in "menu-bar" package
(use-package menu-bar
  :ensure nil
  :general
  ("<f10> b"      #'menu-bar-mode)
  ("<f10> <f10>"   #'menu-bar-open)
  :config
  (menu-bar-mode 0)                             ; no menu bar
  :demand)

;; built-in "face-remap" package
(use-package face-remap
  :ensure nil
  :general
  ("<f10> -" #'text-scale-adjust)
  ("<f10> =" #'text-scale-adjust)
  :init
  (setq text-scale-mode-step 1.05)              ; text size increases by 5% (normally 20%)
  :defer 3)

;; built-in generic mode
(use-package generic
  :ensure nil
  :config
  (define-generic-mode 'selinux-contexts-mode
    '("#") nil nil
    '("file_contexts\\'")
    '( me:run-prog-mode-hooks ))
  (define-generic-mode 'selinux-policy-mode
    '("#") '("type" "allow" "neverallow" ) nil
    '("\\.te\\'")
    '( me:run-prog-mode-hooks ))
  :defer 1)

;; build-in configure file mode
(use-package conf-mode
  :ensure nil
  :hook (conf-mode . me:run-prog-mode-hooks)
  :defer 1)

;; built-in eldoc mode
(use-package eldoc
  :general
  ("<f6> h"  #'eldoc-doc-buffer)
  ("<f10> e" #'eldoc-mode)
  :config
  (global-eldoc-mode t))

;; built-in shortdoc mode
(use-package shortdoc
  :general
  ("<f4> q" #'shortdoc-display-group)
  (:keymaps 'shortdoc-mode-map
            "j" #'shortdoc-next
            "k" #'shortdoc-previous)
  :ensure nil
  :defer 1)

;; built-in tab-line mode
(use-package tab-line
  :ensure nil
  :general
  ("<f10> a" #'tab-line-mode)
  :config
  (defun me:tab-name(tab tabs)
    (let ((ind (seq-position tabs tab)))
      (if (window-system)
          (format "%c %s" (+ ind ?\u2460) (buffer-name tab))
        (format "%d) %s" (1+ ind) (buffer-name tab)))))
  ;; show first 9 buffers only, and show most recent first
  (defun me:filter-tab-line-buffers ( bufs )
    (seq-take (seq-reverse bufs) 9))
  (advice-add #'tab-line-tabs-window-buffers :filter-return #'me:filter-tab-line-buffers)
  (setq tab-line-tab-name-function #'me:tab-name)
  (global-tab-line-mode t)
  :demand)

;; highlight keywords
(use-package fic-mode
  :hook (prog-mode . fic-mode)
  :custom
  (fic-highlighted-words `( "TODO" "HACK" "KLUDGE" "FIXME" "TRICKY" "BUG" )))

(use-package whitespace
  :hook ((prog-mode text-mode c-mode-common) . whitespace-mode )
  :hook ((whitespace-mode me:after-load-theme ) . me:whitespace-after-theme-change)
  :config
  (setq whitespace-line-column nil                      ; highlight past fill-column
        whitespace-style '(face trailing tabs tab-mark space-before-tab)
        whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
  :config
  ;; set font attributes after theme loads
  (defun me:whitespace-after-theme-change ()
    (let ((bg (face-attribute 'default :background))
          (fg (face-attribute 'default :foreground)))
      (if me:theme-is-dark-p
          (set-face-attribute 'whitespace-tab nil :foreground "grey30" :background bg )
        (set-face-attribute 'whitespace-tab nil :foreground "LightGrey" :background bg ))
      (set-face-attribute 'whitespace-trailing nil :foreground fg :background "PaleVioletRed1" ))))

(use-package display-fill-column-indicator
  :hook ((prog-mode text-mode with-editor-mode) . display-fill-column-indicator-mode )
  :hook (me:after-load-theme . me:display-fill-column-indicator-after-theme-change)
  :general
  ("<f10> i"      #'display-fill-column-indicator-mode)
  :custom
  (display-fill-column-indicator-character ?\u2502)
  :config
  (defun me:display-fill-column-indicator-after-theme-change ()
    ;; N.B. DejaVu Sans Mono has the extra long vertical bar which connects
    (if me:theme-is-dark-p
        (if (display-graphic-p)
            (set-face-attribute 'fill-column-indicator nil :foreground "grey30" :font "DejaVu Sans Mono-14")
          (set-face-attribute 'fill-column-indicator nil :foreground "grey30"))
      ;; For console, WhiteSmoke is too light to show, so LightGrey is used
      (if (display-graphic-p)
          (set-face-attribute 'fill-column-indicator nil :background "white" :foreground "WhiteSmoke" :font "DejaVu Sans Mono-14")
        (set-face-attribute 'fill-column-indicator nil :foreground "LightGrey"))))
  :ensure nil)

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode )
  :custom
  (adaptive-wrap-extra-indent 3))

(use-package modus-themes
  :init
  ;; make sure this load theme hook runs first so it can setup variables
  (add-hook 'me:after-load-theme-hook 'me:modus-themes-after-load-theme -100)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mode-line '(moody accented))
  (modus-themes-fringes 'subtle)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-scale-headings 't)
  (modus-themes-headings '((t . (rainbow))))
  :config
  (defun me:modus-themes-after-load-theme ()
    (setq me:theme-is-dark-p (eq (modus-themes--current-theme) 'modus-vivendi)))
  (modus-themes-load-themes)
  ;; initially use dark theme for console
  (if (window-system)
      (modus-themes-load-operandi)
    (modus-themes-load-vivendi))
  :defer 0)

(use-package smart-mode-line
  :demand
  :after (:all (:any modus-operandi-theme modus-vivendi-theme) project )
  :init
  (setq sml/theme 'respectful
        size-indication-mode t
        sml/line-number-format "%4l"
        sml/size-indication-format " %I "
        sml/col-number-format "%3C"
        sml/numbers-separator " "
        sml/no-confirm-load-theme t)
  :config
  (sml/setup))  ;; runs hooks on sml/after-setup-hook

;; add modeline popup
(use-package minions                    ; A minor-mode menu for the mode line
  :hook (sml/after-setup . minions-mode)
  :custom
  (minions-direct '(flycheck-mode overwrite-mode))
  (minions-mode-line-delimiters nil)
  (minions-mode-line-lighter "[☰]")
  :config
  (unless (display-graphic-p)
    (global-set-key [mode-line down-mouse-1] 'minions-minor-modes-menu))
  ;; Reset all buffer's mode line to the default one
  (mapc (lambda (buffer)
          (if (or (buffer-file-name buffer)
                  (not (equal (substring (buffer-name buffer) 0 1) " ")))
              (with-current-buffer buffer
                (kill-local-variable 'mode-line-format)
                (force-mode-line-update t))))
        (buffer-list)))

;; modeline tabs
(use-package moody
  :after (:all (:any modus-operandi-theme modus-vivendi-theme) project )
  :demand
  :custom
  (x-underline-at-descent-line t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; 26.1 built-in line numbers mode
(use-package display-line-numbers
  :config
  (defun me:rotate-line-number-type ()
    (interactive)
    (setq display-line-numbers
          (pcase display-line-numbers
            ((pred null) t)
            ('relative nil)
            (_ 'relative))))
  :general
  ("<f10> l"      #'me:rotate-line-number-type)
  :init
  (setq display-line-numbers-type 'relative))

;; Display difference indicators in margin
(use-package diff-hl
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :commands (hydra-diff-hl/body)
  :config
  (defun me:diff-hl-refresh-buffer ()
    "Refresh diff-hl in the current buffer"
    (interactive)
    (diff-hl-update))
  (diff-hl-margin-mode t)
  ;; Define diff-hl hydra
  (eval '(defhydra hydra-diff-hl (:hint nil)
           "Diff hl"
           ("j" diff-hl-next-hunk "Goto next hunk" :column "Navigation")
           ("k" diff-hl-previous-hunk "Goto previous hunk" )
           ("v" diff-hl-diff-goto-hunk "Show hunk" :column "Operations")
           ("g" me:diff-hl-refresh-buffer "Refresh buffer")
           ("r" diff-hl-revert-hunk "Revert hunk at point or all in region"))))

(use-package ruler-mode
  :general
  ("<f10> u" #'ruler-mode))

;; display emoji :smile:
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; better package manager
(use-package paradox
  :general
  ("<f4> P"     #'list-packages)
  ("<f4> p"     #'me:paradox-upgrade-packages)
  (:keymaps 'paradox-menu-mode-map
            "j" #'paradox-next-entry
            "k" #'paradox-previous-entry)
  :config
  ;; make sure packages are up-to-date before updating
  (defun me:paradox-upgrade-packages ()
    (interactive)
    (package-refresh-contents)
    (paradox-upgrade-packages))
  :init
  (setq paradox-spinner-type 'moon
        paradox-execute-asynchronously nil
        paradox-display-download-count t
        paradox-display-star-count t
        paradox-github-token t                ; Dont't ask, don't integrate
        paradox-automatically-star nil        ; Don't star automatically
        paradox-hide-wiki-packages t))

;; keep .emacs.d clean
;; N.B. Doesn't migrate, start with clean directory
;; N.B. May need to remove {,var}/package-quickstart.el* after clean install
(use-package no-littering
  :demand
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package recentf
  :custom
  (recentf-exclude `(,@recentf-exclude
                     "COMMIT_EDITMSG\\'"
                     ".*-autoloads\\.el\\'"
                     ,tramp-file-name-regexp
                     "[/\\]\\.elpa/"
                     ))
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15)
  :config
  (recentf-mode))

;; save buffer positions
(use-package saveplace
  :after no-littering
  :config
  (setq save-place-forget-unreadable-files nil)
  (save-place-mode t)
  :defer 1)

;; built-in package savehist
(use-package savehist
  :after no-littering
  :ensure nil
  :init
  (setq savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring compile-history))
  :config
  (savehist-mode t)
  :defer 0.5)


;; modal window resizing
(use-package windresize
  :general
  ("<f10> r"      #'windresize)
  (:keymaps 'windresize-map
            "q" #'windresize-exit
            "h" #'windresize-left
            "l" #'windresize-right
            "j" #'windresize-down
            "k" #'windresize-up)
  :config)

(use-package find-file
  :ensure nil
  :general
  ("M-1"     #'me:find-other-file)
  :config
  (defun me:find-other-file (&optional prefix)
    "Find other file e.g. .h <-> .cpp. Use prefix to put in other window."
    (interactive "P")
    (let ((ff-case-fold-search nil)
          (ff-always-try-to-create nil)
          (ff-search-directories '("." "../include" "../inc")))
      (ff-find-other-file prefix t))))

;; remote file editting
(use-package tramp
  :custom
  (tramp-backup-directory-alist backup-directory-alist)     ; keep backups local
  (tramp-verbose 2)                                         ; don't tell us about connections
  (tramp-default-method "sshx")                             ; use sshx by default
  (tramp-terminal-type "dumb")                              ; avoid fancy prompts
  (tramp-chunksize 4050)                                    ; via doc for this var
  :config
  ;; use dash on jh-rvueb
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:.*@jh-rvueb:")
                     "remote-shell" "/bin/dash"))
  :ensure nil)

(use-package dired
  :general
  ("<f4> d"   #'dired-jump)
  (:keymaps 'dired-mode-map
            "j" #'dired-next-line
            "k" #'dired-previous-line
            "C-c d" #'dired-hide-details-mode)
  ("*" '(:ignore t :which-key "Marking→" ))
  ("%" '(:ignore t :which-key "Regular Expressions→" ))
  (";" '(:ignore t :which-key "Cryptography→" ))
  (":" '(:ignore t :which-key "Cryptography→" ))
  ("C-t" '(:ignore t :which-key "Images→" ))
  ("M-s" '(:ignore t :which-key "Incremental search→" ))
  ("C-x" '(:ignore t :which-key "Miscellaneous" ))
  :hook (dired-mode . dired-hide-details-mode)
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh1vG --group-directories-first"    ; human readable sizes
        dired-auto-revert-buffer t       ; revert buffer on revisit
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :ensure nil)

;; implement empty subdir collapsing
(use-package dired-collapse
  :hook (dired-mode  . dired-collapse-mode))

;; implement dired-narrow function
(use-package dired-narrow
  :init
  :general
  (:keymaps 'dired-mode-map
            "/" #'dired-narrow)
  :config)

;; inline subtree expand/collapse
(use-package dired-subtree
  :general
  (:keymaps 'dired-mode-map
            "<backtab>" #'dired-subtree-cycle
            "<tab>" #'dired-subtree-toggle))

;; edit dired buffers
(use-package wdired
  :init
  (setq wdired-allow-to-change-permissions t)
  :general
  (:keymaps 'dired-mode-map
            "C-c w" #'wdired-change-to-wdired-mode)
  :ensure nil)

(use-package peep-dired
  :hook ( peep-dired . me:peep-dired-config )
  :init
  (setq peep-dired-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "j")         #'peep-dired-next-file)
          (define-key map (kbd "k")         #'peep-dired-prev-file)
          (define-key map (kbd "C-f")       #'peep-dired-scroll-page-down)
          (define-key map (kbd "C-b")       #'peep-dired-scroll-page-up)
          (define-key map (kbd "q")         #'peep-dired)
          map))
  :config
  (defun me:peep-dired-config ()
    (evil-make-intercept-map peep-dired-mode-map 'normal)
    (evil-normalize-keymaps 'normal))
  :general
  (:keymaps 'dired-mode-map
            "C-c p" #'peep-dired))

(use-package dired-sidebar
  :hook (dired-sidebar-mode . me:dired-sidebar-config)
  :custom
  (dired-sidebar-subtree-line-prefix "  ")
  (dired-sidebar-theme 'nerd)
  (dired-sidebar-close-sidebar-on-file-open t)
  :general
  ("<f7> h"   #'dired-sidebar-toggle-sidebar)
  ("<f4> h"   #'dired-sidebar-toggle-with-current-directory)
  :config
  (defun me:dired-sidebar-config ()
    (unless (file-remote-p default-directory)
      (auto-revert-mode)))
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))


(use-package flyspell
  :unless (eq window-system 'w32)
  :hook (prog-mode . flyspell-prog-mode)
  :hook (text-mode . flyspell-mode)
  :general
  ;; N.B. C-; is flyspell-auto-correct-previous-word
  ("C-M-;"      #'flyspell-correct-word-before-point)
  :init
  (setq ispell-personal-dictionary (expand-file-name "hunspell/words" me:config-directory))
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
  )

;; look up words
(use-package define-word
  :general
  (:keymaps 'normal :prefix "SPC"
            "d" '(define-word-at-point :which-key "Define this word")
            "D" '(define-word :which-key "Define word"))
  :config
  ;; advise define-word to set word limit based on frame height
  (defun me:advise-define-word (_word _service &optional _choose)
    (setq define-word-limit (/ (+ 2 (frame-height)) 3)))
  (advice-add #'define-word :before #'me:advise-define-word))

;; vertical completion
(use-package vertico
  :general
  ("<f4> <f4>" #'vertico-repeat)
  :hook ( minibuffer-setup . vertico-repeat-save )
  :defer 1
  :config
  (vertico-mode)
  (vertico-indexed-mode)
  (setq vertico-count 20
        vertico-resize nil))

;; easier directory navigation
(use-package vertico-directory
  :after vertico
  ;; part of vertico, so don't try to download
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; match multiple regexps in any order
(use-package orderless
  :demand t
  :after vertico
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles partial-completion)))))

;; completion-read functions
(use-package consult
  :after vertico
  :init
  ;; for some reason it looks like shell expansion is going on with consult-find
  ;; and putting single quotes around the -wholename argument this fixes it
  ;; without special-casing windows but the developer is kind of a butthead and
  ;; won't do this. On the other hand, it's not clear where the expansion is
  ;; happening, so he may have a point.
  (setq consult-find-args "find . -not ( -wholename '*/.*' -prune )")
  :general
  ("<f2>"  #'consult-buffer)
  ("<f4> f"  #'consult-find)
  ("<f4> /"  #'consult-focus-lines)
  ("M-<f2>"  #'me:find-window-buffer)
  ("M-y"    #'consult-yank-from-kill-ring)
  ("<f10> t" #'consult-theme)
  ("<f10> c" #'read-color)
  ("<help> a"    #'consult-apropos)
  ("<f6> m" #'consult-imenu)
  ("<f6> M" #'consult-imenu-multi)
  (:keymaps 'global :prefix "<f4>"
            "a" #'consult-apropos
            "m" #'consult-mark
            "j" #'consult-bookmark
            "l" #'consult-line
            "s" #'me:consult-ripgrep-here
            "u" #'insert-char
            "o" #'consult-locate)

  :config
  (setq xref-show-xrefs-function #'consult-xref)

  ;; find file using fd
  (defun me:consult-find-fd (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fd" "--color=never" "--full-path")))
      (consult-find dir)))
  ;; search using ripgrep
  (defun me:consult-ripgrep-here ()
    "Search using ripgrep in default directory"
    (interactive)
    (consult-ripgrep default-directory (thing-at-point 'symbol)))
  (defun me:find-window-buffer()
    (interactive)
    "Find buffer from buffers previously used in window"
    (when-let* ((allbufs (mapcar 'car (window-prev-buffers)))
                (bufs (remove (current-buffer) allbufs))
                (cands (mapcar #'buffer-name bufs))
                (buf-name (completing-read
                           "Buffer: " cands
                           nil t "" 'buffer-name-history
                           (buffer-name (other-buffer (current-buffer))) t)))
      (switch-to-buffer buf-name))))

(use-package embark
  :demand t
  :after vertico
  ;; TODO convert to general
  :bind (:map minibuffer-local-map
              ("C-o" . embark-act)
              ("C-c C-o" . embark-act)
              ("C-c C-c" . embark-dwim)
              :map embark-file-map
              ("j" . dired-jump)))

(use-package embark-consult
  :demand t
  :after ( embark consult ))

;; allow grep buffers to be editted
(use-package wgrep
  :general
  (:keymaps 'grep-mode-map "C-c w" #'wgrep-change-to-wgrep-mode))

;; built-in project.el
(use-package project
  :demand t
  :general
  ("<f3>"  #'project-find-file)
  ;; TODO: This duplicates project-prefix-map; should find a way to use that
  (:prefix "<f7>"
           "<f7>" 'me:rg-project
           "!" 'project-shell-command
           "&" 'project-async-shell-command
           "f" 'project-find-file
           "F" 'project-or-external-find-file
           "b" 'project-switch-to-buffer
           "s" 'project-shell
           "d" 'project-find-dir
           "D" 'project-dired
           "v" 'project-vc-dir
           "c" 'project-compile
           "e" 'project-eshell
           "k" 'project-kill-buffers
           "p" 'project-switch-project
           "g" 'project-find-regexp
           "G" 'project-or-external-find-regexp
           "r" 'project-query-replace-regexp
           "x" 'project-execute-extended-command)
  :config
  (defun me:rg-project ()
    "Search using ripgrep in project"
    (interactive)
    (if-let ((root (me:project-path)))
        (consult-ripgrep root (thing-at-point 'symbol))
      (message "Not in a project")))
  ;; Add project relative org templates
  (defun me:add-project-templates ()
    (when (project-current)
      (add-to-list 'org-capture-templates
                   `("pn" "Project Notes" entry (file+headline ,(me:project-path "Notes/notes.org") "Notes")))
      (add-to-list 'org-capture-templates
                   `("pd" "Project Tasks" entry (file+headline ,(me:project-path "Notes/notes.org") "TODOs")
                     "* TODO %?\n  %i\n  %a"))))
  ;; Return path relative to project or nil if no project
  (defun me:project-path ( &optional path)
    (if-let* ((proj (project-current))
              (root (project-root proj)))
        (if path
            (expand-file-name path root)
          root)))
  (defun me:project-mode-line-info()
    (if-let* ((path (me:project-path))
              (name (file-name-nondirectory (directory-file-name path))))
        (concat " [" name "] ")))
  (add-to-list 'mode-line-misc-info `(:eval (me:project-mode-line-info)))
  :defer 0.5)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode) . rainbow-delimiters-mode ))

;; color color strings
(use-package rainbow-mode
  :hook (( emacs-lisp-mode js-mode ) . rainbow-mode ))

;; Highlight cursor's surrounding parentheses
(use-package highlight-parentheses
  :hook ( prog-mode . highlight-parentheses-mode ))

(use-package aggressive-indent
  :hook ( emacs-lisp-mode . aggressive-indent-mode))

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package org
  :hook (org-capture-mode . evil-insert-state)
  :hook (org-babel-after-execute . me:redisplay-inline-images)
  :commands org-capture
  :custom
  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  :general
  ("<f8> t" #'org-todo-list)
  ("<f8> a" #'org-agenda)
  ("<f8> c" #'me:org-capture)
  (:prefix "C-c"
           "C-x"  '(:ignore t :which-key "Org→" ))
  :init
  (defconst me:command (expand-file-name "Notes/command.org" me:data-directory))
  (defconst me:language (expand-file-name "Notes/language.org" me:data-directory))
  (defconst me:system (expand-file-name "Notes/system.org" me:data-directory))
  (defconst me:android (expand-file-name "Notes/android.org" me:data-directory))
  (defconst me:home-notes (expand-file-name "Notes/notes.org" "~"))
  (setq org-confirm-babel-evaluate #'me:babel-should-confirm
        org-plantuml-jar-path (expand-file-name "java/plantuml.jar" me:data-directory)
        org-ascii-bullets '((ascii 42) (latin1 167) (utf-8 8226))
        org-return-follows-link t
        org-src-window-setup 'current-window
        org-startup-indented t
        org-ascii-headline-spacing '(0 . 0))
  (setq org-capture-templates nil)
  (setq me:org-capture-templates `(("to" "General Tasks" entry (file+headline ,me:home-notes "Tasks") "* TODO %?\n  %i\n  %a")
                                   ("no" "General Notes" entry (file+headline ,me:home-notes "Notes"))
                                   ("gi" "Using Git" entry (file+headline ,me:command "Git"))
                                   ("li" "Using Linux" entry (file+headline ,me:command  "Linux" ))
                                   ("ba" "Using Bash" entry (file+headline ,me:command  "Bash" ))
                                   ("la" "Administrating Linux " entry (file+headline ,me:system "Linux"))
                                   ("ma" "Administrating Mac" entry (file+headline ,me:system "Mac"))
                                   ("el" "Elisp" entry (file+headline ,me:language  "Elisp" ))
                                   ("py" "Python" entry (file+headline ,me:language  "Python" ))
                                   ("c+" "C++" entry (file+headline ,me:language "C++" ))
                                   ("ag" "Android General" entry (file+headline ,me:android  "General" ))
                                   ("ab" "Android Build System" entry (file+headline ,me:android  "Build System" ))
                                   ("aa" "Android Architecture" entry (file+headline ,me:android  "Architecture" ))))

  :mode
  (("\\.org\\'" . org-mode))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t) (ditaa . t)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (defun me:redisplay-inline-images ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))
  (defun me:babel-should-confirm (lang body)
    (not (member lang '( "plantuml" "ditaa" ))))
  (defun me:org-capture ()
    (interactive)
    (setq org-capture-templates me:org-capture-templates)
    (me:add-project-templates)
    (counsel-org-capture)))

;; search multiple notes directories
(use-package consult-notes
  :general
  ("<f8> <f8>" #'me:search-all-notes)
  :config
  (defun me:search-all-notes()
    (interactive)
    (let* ((consult-notes-file-dir-sources
            `(("dot" ?d ,(expand-file-name "Notes" me:data-directory))
              ("proj" ?p ,(me:project-path "Notes"))
              ("home" ?h ,(expand-file-name "Notes" "~")))))
      (consult-notes-search-in-all-notes)))
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package valign
  :hook (markdown-mode . valign-mode)
  :custom (valign-fancy-bar t))

(use-package evil-org
  :after ( org evil )
  :hook (org-mode . evil-org-mode)
  :general
  ;; workaround for org link RET in normal mode
  ;; see https://github.com/Somelauw/evil-org-mode/issues/57
  (:keymaps 'motion "<RET>" nil)
  :init
  (add-hook 'evil-org-mode-hook (lambda ()
                                  (evil-org-set-key-theme
                                   '( navigation insert  textobjects additional calendar return ))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package rustic
  :hook ( rustic-mode . me:rustic-mode-config )
  :config
  (defun me:rustic-mode-config ()
    (flyspell-mode -1) ; really slows down scrolling not useful enough to keep
    (setq-local buffer-save-without-query t))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-lsp-server 'rust-analyzer)
  (rustic-compile-backtrace 1)      ; sets RUST_BACKTRACE=1
  (rustic-match-angle-brackets nil)   ; t slows down scrolling a lot
  (rustic-compile-display-method (lambda (buf) (display-buffer-pop-up-window buf nil))))

(use-package toml-mode)
(use-package protobuf-mode
  :custom
  (indent-tabs-mode t))

(use-package cmake-mode
  :init
  (setq cmake-tab-width 3)
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

;; built-in makefile mode
(use-package make-mode
  :ensure nil
  :hook ( makefile-mode . me:makefile-mode-config )
  :config
  (defun me:makefile-mode-config ()
    (modify-syntax-entry ?+ "." )))                  ; + is punctuation

;; n.b. buffer-face-mode screws up completion popups
;; may be fixed in 25.1 or so
(use-package adoc-mode
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case)
  :config
  (add-to-list 'ispell-skip-region-alist '( "\n\\[latexmath\\]\n\\+\\+\\+\\+\n" . "^\\+\\+\\+\\+\n"))
  (add-to-list 'ispell-skip-region-alist '( "\n:" . "\n"))
  (defun me:adoc-mode-config ()
    (setq company-dabbrev-downcase nil     ; don't downcase completions
          company-dabbrev-ignore-case nil))  ; don't keep prefix
  :mode
  (("\\.ad\\'" . adoc-mode)
   ("\\.adoc\\'" . adoc-mode)
   ("\\.asciidoc\\'" . adoc-mode))
  :hook (adoc-mode . me:adoc-mode-config))

;; math preview mode for adoc mode
(use-package math-preview
  :custom
  (math-preview-scale 1.2)
  :general
  ("C-c C-m" #'math-preview-all)
  (:keymaps 'normal :prefix "SPC"
            "m" '(math-preview-at-point :which-key "Math preview at point"))
  :config
  (add-to-list 'math-preview-tex-marks-inline `("stem:[" "]"))
  ;; workaround for duplicate label, see issue #21
  (add-to-list
   'math-preview-tex-preprocess-functions
   `(lambda (x) (puthash 'string (s-replace-regexp "\\label{.+?}" "" (gethash 'string x)) x)) t))

;; built-in restructured text mode
(use-package rst-mode
  :ensure nil
  :init
  (setq rst-pdf-program "xdg-open"))

(use-package sphinx-mode
  :hook (rst-mode . sphinx-mode))

(use-package plantuml-mode
  :custom
  ;; TODO define the path to plantuml globally and use instead of repeating this code
  (plantuml-jar-path (expand-file-name "java/plantuml.jar" me:data-directory))
  (plantuml-default-exec-mode 'jar)
  :mode
  (("\\.plantuml\\'" . plantuml-mode)))

(use-package python-mode
  :hook (python-mode . me:python-mode-config)
  :defines ( python-indent-offset python-indent-guess-indent-offset )
  :config
  ;; TODO - editorconfig may make some of this obsolete
  (defun me:python-mode-config ()
    (semantic-mode t)
    (setq evil-shift-width 4)
    (setq python-indent-offset 4)
    (setq python-indent-guess-indent-offset t)
    (setq tab-width 4))
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.py3\\'" . python-mode)))

(use-package ruby-mode
  :mode
  (("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
   ("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package js2-mode
  :mode
  (("\\.js\\'" . js2-mode)
   ("\\.json\\'" . js2-mode)))

;; basic setup for java code
(use-package java-mode
  :mode
  (("\\.java\\'" . java-mode))
  (("\\.aidl\\'" . java-mode))                               ; Hack AIDL syntax highlighting
  (("\\.hal\\'" . java-mode))                               ; Hack HIDL syntax highlighting
  :ensure nil)

(use-package cc-mode
  :hook (c++-mode . me:c++-mode-config )
  :hook (c-mode . me:c-mode-config )
  :custom
  (c-electric-pound-behavior (quote (alignleft)))  ; cpp directives aligned to left
  (show-paren-mode 0)                              ; don't visualize matching parens
  (indent-tabs-mode nil)                           ; no tabs
  :config
  (defun me:c-mode-config ()
    ;; ambihelical style - use ellemtel style but use c-basic-offset of 4
    (c-add-style "ambihelical" '("ellemtel" (c-basic-offset . 4)))
    (c-set-style "ambihelical" nil)
    (c-set-offset 'arglist-intro '+)               ; indent args extra
    (c-set-offset 'innamespace [0]))               ; no indentation in namespace
  (defun me:c++-mode-config ()
    (me:c-mode-config)
    (define-key c++-mode-map ":" #'self-insert-command)
    (define-key c++-mode-map ")" #'self-insert-command)
    (define-key c++-mode-map ";" #'self-insert-command))
  :mode
  (("\\.\\(cpp\\|cxx\\|h\\|hpp\\)\\'" . c++-mode)
   ("\\.c\\'"   . c-mode)))

;; i3 configs
(use-package i3wm-config-mode)

;; very large file support
(use-package vlf
  :general
  (:prefix "C-c"
           "C-v"  '(:ignore t :which-key "VLF→" ))
  :defer 0
  :init
  (require 'vlf-setup))

(use-package logview
  :general
  ("C-c c" #'me:colorize-ansi-escapes)
  :config
  (defun me:colorize-ansi-escapes ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  :mode
  (("\\.log\\'"   . logview-mode)))

;; font lock for newer c++ versions
(use-package modern-cpp-font-lock
  :hook ( c++-mode . modern-c++-font-lock-mode ))

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
            "C-c C-r" #'me:rotate-skip-threshold
            "<SPC>" nil
            "g" nil
            "j" nil
            "k" nil
            "h" nil
            "l" nil)
  :custom
  (compilation-skip-threshold 2)  ;; skip below error
  :config
  (defun me:switch-to-compile-buffer ()
    (interactive)
    (switch-to-buffer compilation-last-buffer))
  (defun me:rotate-skip-threshold ()
    (interactive)
    (compilation-set-skip-threshold
     (cond ((= compilation-skip-threshold 1) 2)
           ((= compilation-skip-threshold 2) 0)
           (t 1))))
  (defun me:compile-finish (buf str)
    (compilation-set-skip-threshold 1)
    (when (and (display-graphic-p) (not (eq window-system 'w32)))
      (x-urgency-hint (selected-frame))))

  :init
  (setq compilation-scroll-output t
        compilation-ask-about-save nil                 ; save all modified
        compilation-always-kill t                      ; always kill existing process
        compilation-auto-jump-to-first-error t
        compilation-finish-functions #'me:compile-finish)
  (add-hook 'compilation-start-hook
            (lambda (_proc) (compilation-set-skip-threshold 2))))

;; view symbols of libraries
(use-package elf-mode
  :magic
  (("ELF" . elf-mode)))

(use-package woman
  :init
  (setq woman-use-topic-at-point t                          ; man page on word at point if exists
        Man-notify-method 'aggressive)                      ; show&select man page in other window
  :general
  ("<f4> m" #'woman)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
  :ensure nil)

(use-package doc-view
  :init
  (setq doc-view-continuous t
        doc-view-resolution 144)
  :general
  (:keymaps 'doc-view-mode-map
            "j" #'doc-view-next-line-or-next-page
            "k" #'doc-view-previous-line-or-previous-page
            "h" #'image-backward-hscroll
            "l" #'image-forward-hscroll)
  :ensure nil)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

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
                                        ; first extension in list is used for new files.
        deft-extensions '("md" "txt" "text" "markdown" "mmd" "org")
                                        ; deft auto-save interferes with whitespace-butler, so disable
        deft-auto-save-interval 0)
  (add-hook 'deft-mode-hook
            (lambda ()
              (define-key deft-mode-map (kbd "<f4> n") #'quit-window)
              (define-key deft-mode-map (kbd "<C-return>") #'deft-new-file)
              (define-key deft-mode-map (kbd "<C-backspace>") #'deft-filter-clear))))

;; polyglot language server interface
(use-package eglot
  :after no-littering
  :general
  (:keymaps 'eglot-mode-map
            "<f6> x"  #'eglot-rename
            "C-<tab>" #'complete-symbol
            "<f6> c"  #'eglot-code-actions)
  :init
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider)
        eglot-send-changes-idle-time 3    ;; be slower sending changes
        eglot-extend-to-xref t            ;; external files ok
        eglot-events-buffer-size 100000)  ;; smaller events buffer
  ;; windows eglot has a bug with eglot-extend-to-xref. See issue 715
  (when (eq window-system 'w32)
    (setq eglot-extend-to-xref nil))
  :config
  ;; configure clangd for c++ and c
  (when-let* ((clangd (seq-find #'executable-find '("clangd" "clangd-6.0")))
              ;; this has to match the tool string in compile-commands.json
              ;; clangd will then use these tools to get system header paths
              (init-args "--query-driver=/**/*"))
    (when (eq window-system 'w32)
      (setq init-args "--query-driver=*:\\**\\*"))
    (add-to-list 'eglot-server-programs
                 `((c++-mode c-mode) ,clangd ,init-args)))
  :hook ((rust-mode c++-mode c-mode) . eglot-ensure))

(use-package eldoc-box
  :after eglot
  :demand t
  :custom
  (eldoc-box-max-pixel-width 1200)
  :config
  ;; remove problematic markdown text
  ;; 1. Trailing spaces, triggers whitespace mode
  ;; 2. Horizontal rulers, not render well
  (defun me:eglot-sanitize-markdown(str)
    (replace-regexp-in-string
     "\s+\n" "\n"
     (string-replace "\n---\n" "" str)))
  ;; fix horizontal ruler in markdown rendering
  (defun me:eglot--format-markup (args)
    (mapcar (lambda (markup)
		      (if-let* ((value (plist-get markup :value))
			            (kind (plist-get markup :kind))
			            (_ (string= kind "markdown"))
			            (value (me:eglot-sanitize-markdown value)))
		          (plist-put markup :value value))
		      markup)
	        args))
  :init
  (advice-add 'eglot--format-markup :filter-args 'me:eglot--format-markup)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))

(use-package company :demand t)

(use-package corfu
  :demand t
  :after vertico
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-separator ?\s)          ;; Orderless field separator
  :config
  (defun me:corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (evil-ex-p))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'me:corfu-enable-in-minibuffer 1)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :config
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up))  ;; corfu-previous

;; built-in package for cross-references
(use-package xref
  :config
  (add-to-list 'xref-prompt-for-identifier #'xref-find-references t)
  :general
  ("<f6> <f6>" #'xref-find-definitions)
  ("<f6> d"    #'xref-find-definitions-other-window)
  ("<f6> r"   #'xref-find-references)
  ("<f6> a"   #'xref-find-apropos))

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :general
  ("M-<SPC>" #'yas-expand)  ;; was just-one-space, a fairly useless binding
  ("<f4> y" #'yas-insert-snippet)
  ("<f4> Y" #'yas-describe-tables)
  (:prefix "C-c"
           "&" '(:ignore t :which-key "Yasnippet→" ))
  :custom
  (yas-prompt-functions '( yas-completing-prompt ))
  :init
  (add-hook 'yas-before-expand-snippet-hook         ; evil-insert at each slot
            (lambda()
              (let ((p (point)) (m (mark)))
                (evil-insert-state)
                (goto-char p)
                (set-mark m))))
  :config
  (use-package yasnippet-snippets)
  (define-key yas-minor-mode-map (kbd "<tab>") nil) ; don't use <tab>
  (define-key yas-minor-mode-map (kbd "TAB") nil))   ; don't use TAB

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-mode-line-prefix "☑")
  (flycheck-idle-change-delay 3)     ;; default of 0.5s is too noisy
  :general
  (:prefix "C-c"
           "!"  '(:ignore t :which-key "Flycheck→" )))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :custom
  (flycheck-pos-tip-timeout 3))

;; enable code folding (evil has bindings)
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :general
  (:prefix "C-c"
           "@" '(:ignore t :which-key "HideShow→" )))

(use-package avy
  :commands ( avy-goto-word-1 avy-goto-char-2 avy-goto-char-in-line )
  :init
  (setq avy-all-windows 'all-frames))

(use-package evil
  :hook (( prog-mode text-mode ) . evil-local-mode)
  :hook ( evil-local-mode . me:evil-local-mode-config)

  :custom
  (evil-want-keybinding nil)  ; use evil-collection instead (needs to be done early)
  (evil-respect-visual-line-mode t)   ; movements respect visual line mode
  (evil-want-C-u-delete t)              ; use vim C-u binding (M-u replaces)
  (evil-want-C-u-scroll t)             ; use vim C-u binding (M-u replaces)
  ;; N.B. This needs to be after evil-want-C-u-delete to work
  (evil-disable-insert-state-bindings t) ; allow emacs bindings in insert mode
  (evil-undo-system 'undo-fu)

  :init
  (setq-default evil-symbol-word-search t   ; misnamed: t is search for symbols, not words
                evil-shift-width tab-width)         ; shift by ideal width :)
  (setq evil-want-C-w-delete nil            ; want C-w for windows commands
        evil-want-C-w-in-emacs-state t      ; ditto
        evil-mode-line-format '( before . mode-line-front-space)
        evil-search-module #'evil-search)

  (setq evil-normal-state-tag   (propertize " N " 'face '((:background "DarkGoldenrod2" :foreground "black")))
        evil-emacs-state-tag    (propertize " E " 'face '((:background "SkyBlue2"       :foreground "black")))
        evil-insert-state-tag   (propertize " I " 'face '((:background "chartreuse3"    :foreground "black")))
        evil-replace-state-tag  (propertize " R " 'face '((:background "chocolate"      :foreground "black")))
        evil-motion-state-tag   (propertize " M " 'face '((:background "plum3"          :foreground "black")))
        evil-visual-state-tag   (propertize " V " 'face '((:background "gray"           :foreground "black")))
        evil-operator-state-tag (propertize " O " 'face '((:background "sandy brown"    :foreground "black"))))
  :general
  (:states '(normal visual) :keymaps 'override
           :prefix "<SPC>"
           ";"          #'evil-jump-forward
           ","          #'evil-jump-backward
           "a"          #'align
           "f"          #'avy-goto-word-1
           "g"          #'avy-goto-char-2
           "<SPC>"      #'avy-goto-char-timer
           "h"          #'hydra-diff-hl/body
           "l"          #'avy-goto-char-in-line
           "p"          #'hydra-paste/body
           "x"          #'exchange-point-and-mark)
  (:states '(normal visual) :prefix "<SPC>" :keymaps 'override
           "s"  (general-simulate-key "\"*" :keymap nil :lookup nil :name me:simulate-selection-reg )
           "c"  (general-simulate-key "\"+" :keymap nil :lookup nil :name me:simulate-clipboard-reg ))
  (:keymaps '(normal visual ) "<escape>" #'keyboard-quit)

  ;; Scroll keeping cursor stationary
  (:keymaps '( normal insert visual ) "C-j" #'evil-scroll-line-up)       ; ^y
  (:keymaps '( normal insert visual )  "C-k" #'evil-scroll-line-down)     ; ^e

  ;; N.B C-u replacement
  (:keymaps 'global "M-u" #'universal-argument)

  ;; visual mode only mapping
  (:keymaps 'visual
            ;; Overload shifts so that they don't lose the selection
            ">"           #'me:evil-shift-right-visual
            "<tab>"       #'me:evil-shift-right-visual
            "<"           #'me:evil-shift-left-visual
            "<backtab>")   #'me:evil-shift-left-visual
  :config
  ;; configuration to run on local mode hook
  (defun me:evil-local-mode-config ()
    ;; Make evil words the same as symbol, so word motion
    ;; works more like vi, especially for snake_case
    (defalias 'forward-evil-word 'forward-evil-symbol)
    ;; make cut/paste more vim-like
    ;; mainly keep emacs cut/paste separate from system clipboard
    ;; in the default case (must use "+ or "* to override)
    ;; This assumes select-enable-clipboard is set to nil as well
    (setq-default interprogram-paste-function nil
                  interprogram-cut-function nil))
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
    (evil-commentary-mode))

  ;; want to start deft in insert mode
  (evil-set-initial-state 'deft-mode 'insert)

  ;; these modes are better in emacs state
  (dolist (mode
           '(dired-mode
             finder-mode
             image-mode
             image-dired-thumbnail-mode
             cquery-tree-mode
             shortdoc-mode
             paradox-menu-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-mode 1))

;; simple multi cursor mode
(use-package evil-multiedit
  :after evil
  :init
  (setq evil-multiedit-follow-matches t)
  :general
  ("M-d" #'evil-multiedit-match-and-next)
  ("M-D" #'evil-multiedit-match-and-prev)
  ("C-M-d" #'evil-multiedit-restore))

(use-package undo-fu
  :after evil)

;; Operator to switch between the following inflections:
;;  1. kebab-case
;;  2. Pascal_Snake
;;  3. snake_case
;;  4. SCREAMING_SNAKE_CASE
;;  5. PascalCase
;;  6. pascalCase
(use-package evil-string-inflection
  :after evil
  :general
  (:states '(normal visual) :keymaps 'override
           :prefix "g"
           "-"          #'evil-operator-string-inflection))

;; change surrounding text
;; add:    ys<text-obj><delim>
;; change: cs<old-delim><new-delim>
;;
(use-package evil-surround
  :after evil
  :defer 2
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :custom
  (evil-collection-setup-minibuffer nil)
  ;; allow evilification for these modes:
  (evil-collection-mode-list
   `(
     ansi-term
     cmake-mode
     dired
     dired-sidebar
     doc-view
     elisp-mode
     eshell
     help
     helpful
     js2-mode
     log-view
     lua-mode
     magit
     magit-todos
     (package-menu package)
     (pdf pdf-view)
     python
     ruby-mode
     xref
     woman))
  :after evil
  :config
  (evil-collection-init)
  :defer 1)

(use-package expand-region
  :demand t
  :after ( evil evil-collection )
  :config
  (defun evil-visual-char-or-expand-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively 'er/expand-region)
      (evil-visual-char)))
  (define-key evil-normal-state-map "v" 'evil-visual-char-or-expand-region)
  (define-key evil-visual-state-map "v" 'evil-visual-char-or-expand-region)
  (define-key evil-visual-state-map (kbd "M-v") 'er/contract-region)
  (define-key evil-visual-state-map [escape] 'evil-visual-char))

(use-package ws-butler
  :hook (( prog-mode text-mode c-mode-common) . ws-butler-mode ))

(use-package which-key
  :hook (me:after-load-theme . me:which-key-after-theme-change)
  :custom
  (which-key-max-description-length 40)
  (which-key-side-window-max-width 0.67)
  (which-key-side-window-max-height 0.5)
  (which-key-sort-order 'which-key-local-then-key-order)
  :config
  ;; set face scaling after theme change, otherwise these get overridden.
  (defun me:which-key-after-theme-change ()
    (defconst me:which-key-scale 0.80)
    (set-face-attribute 'which-key-key-face nil :height me:which-key-scale)
    (set-face-attribute 'which-key-separator-face nil :height me:which-key-scale)
    (set-face-attribute 'which-key-note-face nil :height me:which-key-scale)
    (set-face-attribute 'which-key-special-key-face nil :height me:which-key-scale)
    (set-face-attribute 'which-key-group-description-face nil :height me:which-key-scale)
    (set-face-attribute 'which-key-command-description-face nil :height me:which-key-scale)
    (set-face-attribute 'which-key-local-map-description-face nil :height me:which-key-scale))
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  :defer 0)

(use-package with-editor
  :hook (with-editor-mode . me:with-editor-mode-config)
  :config
  (defun me:with-editor-mode-config ()
    (let* ((empty (and (eq 0 (current-column)) (bolp) (eolp)))
           (branch (magit-get-current-branch)))
      (setq fill-column 70)
      (when empty
        (when branch
          ;; match board-####- branch names and auto insert bracket tag
          (save-match-data
            (if-let* ((match (string-match "^\\([A-Za-z]+\\)-\\([0-9]+\\)-" branch))
                      (board (match-string 1 branch))
                      (ticket (match-string 2 branch)))
                (insert (concat "[" (upcase board) "-" ticket "] ")))))
        (evil-insert-state)))))

(use-package magit
  :after ( evil evil-collection )
  :init
  (setq magit-save-repository-buffers 'dontask                ; save repo modified buffers w/o asking
        magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)
        magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width nil 18)
        magit-section-initial-visibility-alist '(( stashes . hide ))
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-repository-directories `((,(expand-file-name "dev" "~") . 1)))
  :general
  ("<f9> a"     #'magit-commit-amend)
  ("<f9> b"     #'magit-blame)
  ("<f9> B"     #'magit-run-git-gui-blame)
  ("<f9> c"     #'magit-commit)
  ("<f9> d"     #'magit-file-dispatch)
  ("<f9> f"     #'magit-fetch)
  ("<f9> j"     #'magit-checkout)
  ("<f9> l"     #'magit-log-buffer-file)
  ("<f9> L"     #'magit-log-current)
  ("<f9> o"     #'me:magit-open-revision)
  ("<f9> p"     #'magit-push)
  ("<f9> r"     #'magit-rebase)
  ("<f9> y"     #'magit-cherry-pick)
  ("<f9> z"     #'magit-stash)
  ("<f9> !"     #'magit-git-command)
  ("<f9> <f9>"  #'magit-status)
  :config
  ;; I want these for buffer&window switching
  (define-key magit-section-mode-map (kbd "M-1") nil)
  (define-key magit-section-mode-map (kbd "M-2") nil)
  (define-key magit-section-mode-map (kbd "M-3") nil)
  (define-key magit-section-mode-map (kbd "M-4") nil)
  (define-key magit-section-mode-map (kbd "M-5") nil)
  (define-key magit-section-mode-map (kbd "M-6") nil)
  (define-key magit-section-mode-map (kbd "M-7") nil)
  (define-key magit-section-mode-map (kbd "M-8") nil)
  (define-key magit-section-mode-map (kbd "M-9") nil)
  (define-key magit-mode-map (kbd "M-w") nil)
  (define-key magit-mode-map (kbd "C-w") nil)
  (define-key magit-mode-map (kbd "C-c C-m") #'magit-toggle-margin)
  ;; remove time consuming sections, most to least here, for worst repo I use
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (defun me:magit-open-revision (rev arg)
    "Select and open revision of current file, with prefix opens in other window"
    (interactive (list (magit-read-branch-or-commit "Open revision") current-prefix-arg))
    (if arg
        (magit-find-file-other-window rev (buffer-file-name))
      (magit-find-file rev (buffer-file-name)))))

;; git configuration file editting
(use-package git-modes)

(use-package git-timemachine
  :commands ( hydra-timemachine/body )
  :init
  (setq git-timemachine-mode-map (make-sparse-keymap))  ;; override all bindings
  ;; evil-motion-state when in timemachine mode
  (add-hook 'git-timemachine-mode-hook (lambda ()
                                         (when (fboundp 'evil-motion-state)
                                           (evil-motion-state))))
  :config
  ;; Define time machine hydra. Since we allow any command while the
  ;; timemachine is on, some will "break" timemachine. Stick to
  ;; navigation and all should be good.
  (eval '(defhydra hydra-timemachine
           (:hint nil
                  :body-pre (git-timemachine)
                  :foreign-keys run )
           "Time machine"
           ("M-p" #'git-timemachine-show-previous-revision "Previous revision" :column "Navigation")
           ("M-n" #'git-timemachine-show-next-revision "Next revision")
           ("M-c" #'git-timemachine-show-current-revision "Current revision")
           ("C-c C-c" #'git-timemachine-quit "Quit" :color blue )
           ("M-b" #'git-timemachine-blame "Show culprits" :column "Operations")
           ("M-v" #'git-timemachine-show-commit "Show commit")
           ("M-Y" #'git-timemachine-kill-revision "Yank revision")
           ("M-y" #'git-timemachine-kill-abbreviated-revision "Yank abbreviated revision")))
  :general
  (:keymaps 'global :prefix "<f9>"  "t" #'hydra-timemachine/body))

(use-package popper
  :after project
  :ensure t
  :general
  ("<f4> t"     #'eshell)
  ("M-\""      #'popper-toggle-latest)
  ("M-'"        #'popper-cycle)
  ("<f4> <deletechar>" #'popper-kill-latest-popup)
  ("<f4> '"     #'popper-toggle-type)
  :init
  (setq popper-group-function #'popper-group-by-project
        popper-display-control 'user)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*ielm\\*"
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode
          woman-mode
          magit-status-mode
          help-mode
          occur-mode
          grep-mode
          org-mode
          shortdoc-mode
          "^\\*ivy-occur.*\\*" ivy-occur-mode
          "^\\*helpful.*\\*$" helpful-mode
          rustic-compilation-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
