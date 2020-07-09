;;; -*- lexical-binding: t -*-

(defconst me:gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold me:gc-cons-threshold-original)))

;; package management

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/"))
      package-check-signature nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
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
         use-package-verbose nil))                                 ; don't be verbose

;; xdg directories
(defconst me:data-directory (or (getenv "XDG_DATA_HOME") "~/.local/share"))
(defconst me:cache-directory (or (getenv "XDG_CACHE_HOME")  "~/.cache"))
(defconst me:config-directory (or (getenv "XDG_CONFIG_HOME")  "~/.config"))
;; fonts in order of preference
(defconst me:preferred-fonts #'(("Hack" . "Hack-11:autohint=true")
                                ("Fantasque Sans Mono" . "Fantasque Sans Mono-12")
                                ("DejaVu Sans Mono" . "DejaVu Sans Mono-11")))

;; host specific font handling
(when (equal (system-name) "hum")
  (add-to-list 'me:preferred-fonts '("Roboto Mono" . "Roboto Mono-10:autohint=true")))

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


(defun me:extra-setup ()
  (tool-bar-mode 0)                                           ; no tool bar (tool-bar)
  (scroll-bar-mode 0)                                         ; no scroll bar (scroll-bar)
  (mouse-avoidance-mode 'animate)                             ; move mouse pointer out of way (avoid)
  (global-eldoc-mode -1)                                      ; turn off annoying eldoc mode (eldoc)
  (fset 'yes-or-no-p 'y-or-n-p)                               ; change stupid default y/n? y
  (electric-indent-mode +1)                                   ; turn on electric mode globally (electric)
  (delete-selection-mode t)                                   ; pastes delete selection
  (blink-cursor-mode -1)                                      ; don't blink cursor
  (run-at-time "1 hour" 3600 #'clean-buffer-list)             ; clear out old buffers every hour (midnight)
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
    (define-key input-decode-map "\e[63;6u" (kbd "C-?"))))


;; Run programming mode hooks
;; This is used for modes which should trigger programming mode hooks
(defun me:run-prog-mode-hooks () (run-hooks 'prog-mode-hook))

;; configuration I haven't figured out how to wedge into
;; use-package

(setq-default tab-width 4                                   ; preferred tab width
              indent-tabs-mode nil                          ; disable tabs, re-enable selectively
              indicate-empty-lines t                        ; show empty lines at end of buffer
              fill-column 120)                              ; auto-wrap only very long lines
(setq ad-redefinition-action 'accept                        ; turn off 'xyz' got redefined warnings
      confirm-kill-processes nil                            ; don't ask about killing processes at exit
      custom-file "/dev/null"                               ; disable customizations
      debugger-stack-frame-as-list t                        ; show fns as (fn args) instead of fn(args)
      fast-but-imprecise-scrolling t                        ; quick and dirty scrolling
      find-file-visit-truename t                            ; resolve symlinks finding files
      history-length 1000                                   ; length of history
      history-delete-duplicates t                           ; don't allow repeated history
      imenu-max-item-length 200                             ; default of 60 too short for some c++ methods
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
      use-file-dialog nil                                   ; never want gui file dialog
      use-dialog-box nil                                    ; never want dialog box for questions
      scroll-margin 5                                       ; show some lines around cursor when possible
      scroll-conservatively 101                             ; only scroll enough to bring cursor to view
      scroll-down-aggressively 0.01                         ; don't jump when scrolling down
      scroll-up-aggressively 0.01                           ; don't jump when scrolling up
      safe-local-variable-values                            ; allow these values in .dir-locals.el
      '((evil-indent-convert-tabs . t))
      scalable-fonts-allowed t                              ; allow any scalable font
      select-enable-clipboard nil                           ; make cut/paste function correctly (select)
      sentence-end-double-space nil                         ; sentences end with one space
      standard-indent tab-width                             ; preferred indent
      x-gtk-use-system-tooltips nil)                        ; allow tooltip theming

(add-hook 'after-init-hook                                  ; report init time
          (lambda ()
            (message "Time to initialize: %s"
                     (emacs-init-time))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)                        ; tabs are needed
            (modify-syntax-entry ?+ "." )))                  ; + is punctuation

(run-with-idle-timer 0.1 nil #'me:extra-setup)


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
  :commands ( helpful-callable helpful-key helpful-function helpful-variable helpful-symbol )
  :general
  ("C-h x" #'helpful-callable)
  ([remap describe-key] #'helpful-key)
  ([remap describe-function] #'helpful-function)
  ([remap describe-variable] #'helpful-variable)
  ([remap describe-symbol] #'helpful-symbol)
  :config)

(use-package hydra
  :commands (defhydra)
  :config
  :init
  (setq hydra-base-map (make-sparse-keymap)))

;; infrequently used functions
(use-package extras
  :commands (hydra-paste/body x-urgency-hint)
  :ensure nil
  :general
  ("M-1"     #'me:find-other-file)
  ("<f10> s"  #'me:read-fill-column)
  ("<f4> 1"  #'me:ps-one-per-page)
  ("<f4> 2"  #'me:ps-two-per-page)
  :config
  ;; Define paste hydra.
  ;; eval to avoid pulling in hydra via macro expansion
  ;; Note would like to also redefine C-n, C-p, but these require
  ;; last-command to be a paste, and using a hydra messes with that,
  ;; last-command will be hydra-paste/body
  (eval '(defhydra hydra-paste (:hint nil)
           "Pasting (see also C-n, C-p)"
           ("b" me:paste-then-earlier "Paste above, earlier kill" :column "")
           ("a" me:paste-then-later "Paste below, later kill" )
           ("y" me:counsel-yank-pop-preselect-last "Select from kill ring")))
  :load-path "lisp/")

;; built-in emacs-lisp-mode package
(use-package emacs-lisp-mode
  :ensure nil
  :general
  (:keymaps 'emacs-lisp-mode-map :prefix "C-c"
            "v"          #'pp-eval-last-sexp
            "x"          #'pp-macroexpand-last-sexp)
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (modify-syntax-entry ?- "w")))                  ; hyphens are parts of words
  (add-hook 'emacs-lisp-mode-hook
            (defun me:lexical-binding ()
              (when (and (bobp) (eobp))
                (setq lexical-binding t)
                (insert ";;; -*- lexical-binding: t; -*-\n\n"))))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package highlight-function-calls
  :init
  :config
  ;; more subtle highlighting for functions
  (set-face-attribute 'highlight-function-calls-face nil :inherit font-lock-type-face :underline nil :italic t )
  :hook ( emacs-lisp-mode . highlight-function-calls-mode))

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
  ("M-2"        #'me:window-2nd-buffer)
  ("M-3"        #'me:window-3rd-buffer)
  ("M-4"        #'me:window-4th-buffer)
  ("M-5"        #'me:window-5th-buffer)
  :init
  (add-hook 'focus-out-hook #'me:save-dirty-buffers)          ; save on defocus
  (add-hook 'after-make-frame-functions #'me:set-frame-face)
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

  ;; save any dirty buffers
  (defun me:save-dirty-buffers ()
    "Save any dirty buffers"
    (interactive)
    (save-some-buffers t))

  (defun me:set-frame-face (frame)
    "Set font face for frame"
    (select-frame frame)
    (if (display-graphic-p)
        (let* ((fonts (font-family-list))
               (match (seq-find (lambda (elem) (member (car elem) fonts)) me:preferred-fonts)))
          (if match
              (let ((font-name (cdr match)))
                                        ;(message "font name is %s" font-name)
                (add-to-list 'default-frame-alist `(font . ,font-name))
                (set-frame-font font-name t t))))))

  (defun me:window-nth-buffer (arg &optional prefix)
    "Select the nth other buffer. Use prefix to put in other window"
    (interactive "P")
    (when-let* ((bufs (mapcar 'car (window-prev-buffers)))
                (buffer (nth arg (remove (current-buffer) bufs))))
      (if prefix
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer))))

  (defun me:window-2nd-buffer (&optional prefix)
    "Select 1st other buffer"
    (interactive "P")
    (me:window-nth-buffer 0 prefix))

  (defun me:window-3rd-buffer (&optional prefix)
    "Select 2nd other buffer"
    (interactive "P")
    (me:window-nth-buffer 1 prefix))

  (defun me:window-4th-buffer (&optional prefix)
    "Select 3rd other buffer"
    (interactive "P")
    (me:window-nth-buffer 2 prefix))

  (defun me:window-5th-buffer (&optional prefix)
    "Select 4th other buffer"
    (interactive "P")
    (me:window-nth-buffer 3 prefix))

  (push '("/home-local/" . "/home/") directory-abbrev-alist)
  (push '("/home-remote/" . "/home/") directory-abbrev-alist)
  (mapc #'me:set-frame-face (frame-list))

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

;; built-in minibuffer package
(use-package minibuffer
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)                        ; allow recursive edit
  (add-hook 'minibuffer-setup-hook #'me:minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'me:minibuffer-exit)
  (add-hook 'mouse-leave-buffer-hook #'me:minibuffer-kill)
  :config
  (minibuffer-depth-indicate-mode t)       ; show recursive edit depth (mb-depth)

  ;; value of gc-cons-threshold to restore after minibuffer
  (defvar me:normal-gc-cons-threshold gc-cons-threshold)

  ;; inhibit messages in echo area when minibuffer enabled
  ;; and set use large gc-cons-threshold
  ;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  (defun me:minibuffer-setup ()
    (when (= (minibuffer-depth) 1)
      (setq me:normal-gc-cons-threshold gc-cons-threshold))
    (setq inhibit-message t
          gc-cons-threshold most-positive-fixnum))

  ;; undo that done in me:minibuffer-setup
  (defun me:minibuffer-exit ()
    (when (= (minibuffer-depth) 1)
      ;;(message "gc thresh is %s setting to %s" gc-cons-threshold me:normal-gc-cons-threshold)
      (setq gc-cons-threshold me:normal-gc-cons-threshold))
    (setq inhibit-message (> (minibuffer-depth) 1)))

  ;; kill the minibuffer
  (defun me:minibuffer-kill ()
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))

  :demand)

;; built-in autorevert package
(use-package autorevert
  :hook (( prog-mode text-mode ) . auto-revert-mode )
  :ensure nil
  :defer 2
  :init
  ;; refresh vc state periodically for current buffer -- keep branch valid
  (run-with-timer 15 15 #'vc-refresh-state)
  :custom
  ;; N.B. auto-revert-check-vc-info causes vc-refresh-state to be called
  ;; on *every* file-backed buffer every 5s by default even if it isn't reverted.
  ;; This causes vc-mode-line to be called for each such buffer.
  ;; For this reason, never set this variable.
  (auto-revert-check-vc-info nil)             ; don't check periodically for new vc info
  (auto-revert-verbose nil)                   ; don't tell me about auto reverts
  :config
  :diminish auto-revert-mode)

;; built-in "simple" package
(use-package simple
  :hook (( prog-mode text-mode ) . visual-line-mode )
  :ensure nil
  :general
  ("M-n"        #'next-error)
  ("M-p"        #'previous-error)
  ("C-M-<backspace>"    #'kill-current-buffer)
  :init
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
  :demand)

;; built-in "artist-mode" package
;; No binding because this one will be rare
(use-package artist-mode
  :ensure nil
  :init
  ;; N.B. This assumes we invoke artist mode while in evil normal mode
  (add-hook 'artist-mode-hook
            (lambda ()
              (if artist-mode
                  ( evil-emacs-state t)
                (evil-normal-state t))))
  :general
  (:prefix "C-c"
           "C-a"  '(:ignore t :which-key "Artist→" )))

;; built-in "hl-line" package
(use-package hl-line
  :ensure nil
  :general
  ("<f10> h" #'global-hl-line-mode)              ; toggle hl-line
  :init
  :config
  (set-face-attribute 'hl-line nil :background "grey24")
  (global-hl-line-mode t)                       ; highlight current line (hl-line)
  :defer 1)

;; built-in "menu-bar" package
(use-package menu-bar
  :ensure nil
  :general
  ("<f10> b"      #'menu-bar-mode)
  ("<f10> <f10>"   #'menu-bar-open)
  :init
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
  :config
  :defer 3)

;; built-in prog-mode
(use-package prog-mode
  :ensure nil
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")))                 ; underscores are parts of words
  :config)

;; built-in generic mode
(use-package generic
  :ensure nil
  :init
  :config
  (define-generic-mode 'selinux-contexts-mode
    '("#") nil nil
    '("file_contexts\\'")
    '( me:run-prog-mode-hooks ))
  (define-generic-mode 'selinux-policy-mode
    '("#") '("type" "allow" "neverallow" ) nil
    '("\\.te\\'")
    '( me:run-prog-mode-hooks ))
  :demand)

;; build-in configure file mode
(use-package conf-mode
  :ensure nil
  :init
  (add-hook 'conf-mode-hook #'me:run-prog-mode-hooks)
  :config
  :demand)

;; built-in eldoc mode
(use-package eldoc-mode
  :ensure nil
  :general
  ("<f10> e" #'eldoc-mode))

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
  (me:add-hook-with-delay 'prog-mode-hook 3 #'whitespace-mode)
  (me:add-hook-with-delay 'text-mode-hook 3 #'whitespace-mode)
  :config
  ;; set font attributes after theme loads
  (defun me:set-extra-font-attributes ()
    (let ((bg (face-attribute 'default :background))
          (fg (face-attribute 'default :foreground)))
      (set-face-attribute 'whitespace-line nil :foreground 'unspecified :background "grey24")
      (set-face-attribute 'whitespace-tab nil :foreground "grey32" :background bg )
      (set-face-attribute 'whitespace-trailing nil :foreground fg :background "PaleVioletRed1" )))

  :diminish whitespace-mode)

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
  :init
  (add-hook 'magit-mode-hook
            (lambda ()
              (set-face-attribute 'magit-diff-hunk-heading nil :box "#5e5e5e" :background "dark slate grey")
              (set-face-attribute 'magit-diff-hunk-heading-highlight nil :box "#5e5e5e" :background "steel blue")))
  :config
  ;; make visual and highlight more noticable
  (set-face-attribute 'lazy-highlight nil :background "#5e5e5e")
  (set-face-attribute 'region nil :background "#5e5e5e")
  (set-face-attribute 'highlight nil :background "#5e5e5e")
  :defer 0)


(use-package smart-mode-line
  :demand
  :after hc-zenburn-theme
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
  :init
  (setq minions-direct '(flycheck-mode overwrite-mode)
        minions-mode-line-delimiters nil
        minions-mode-line-lighter "[☰]")
  (add-hook 'sml/after-setup-hook #'minions-mode)
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
  :after ( hc-zenburn-theme perspective )
  :demand
  :init
  (setq x-underline-at-descent-line t)
  :config
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil))
  ;; make mode lines more noticable
  (set-face-attribute 'mode-line nil :background "#4e4e4e" :height 1.1)
  (set-face-attribute 'mode-line-inactive nil :background "#3e3e3e" :height 0.9)
  (set-face-attribute 'persp-selected-face nil :inherit nil)  ; theme fix for inactive modeline
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
            (t 'relative))))
  :general
  ("<f10> l"      #'me:rotate-line-number-type)
  :init
  (setq display-line-numbers-type 'relative))

;; Display difference indicators in margin
(use-package diff-hl
  :hook ((prog-mode text-mode) . diff-hl-mode)
  :commands (hydra-diff-hl/body)
  :init
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; NB: this doesn't work with dired-subtree, so disabling for now
  ;; (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  :config
  (diff-hl-margin-mode t)
  ;; Define diff-hl hydra
  (eval '(defhydra hydra-diff-hl (:hint nil)
           "Diff hl"
           ("j" diff-hl-next-hunk "Goto next hunk" :column "Navigation")
           ("k" diff-hl-previous-hunk "Goto previous hunk" )
           ("v" diff-hl-diff-goto-hunk "Show hunk" :column "Operations")
           ("r" diff-hl-revert-hunk "Revert hunk at point or all in region"))))

(use-package ruler-mode
  :general
  ("<f10> u" #'ruler-mode)
  :config)

;; better package manager
(use-package paradox
  :general
  ("<f4> P"     #'me:paradox-list-packages)
  (:keymaps 'paradox-menu-mode-map
            "j" #'paradox-next-entry
            "k" #'paradox-previous-entry)
  :config
  ;; make sure packages are up-to-date before updating
  (defun me:paradox-list-packages ()
    (interactive)
    (package-refresh-contents)
    (paradox-list-packages nil))
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
                     "[/\\]\\.elpa/"
                     ))
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 300) ; wait 5m before 1st cleanup
  :config
  (recentf-mode))

;; save buffer positions
(use-package saveplace
  :after no-littering
  :init
  (setq-default save-place t)
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

;; remote file editting
(use-package tramp
  :init
  (setq tramp-terminal-type "dumb"                              ; avoid fancy prompts
        tramp-backup-directory-alist backup-directory-alist     ; keep backups local
        tramp-verbose 2                                         ; don't tell us about connections
        tramp-default-method "ssh")                             ; use ssh by default
  :ensure nil)

(use-package dired
  :general
  ("<f4> d"   #'dired-jump)
  (:keymaps 'dired-mode-map
            "<f5> h" #'hydra-dired/body
            "j" #'dired-next-line
            "k" #'dired-previous-line
            "<f5> d" #'dired-hide-details-mode)
  ("*" '(:ignore t :which-key "Marking→" ))
  ("%" '(:ignore t :which-key "Regular Expressions→" ))
  (";" '(:ignore t :which-key "Cryptography→" ))
  (":" '(:ignore t :which-key "Cryptography→" ))
  ("C-t" '(:ignore t :which-key "Images→" ))
  ("M-s" '(:ignore t :which-key "Incremental search→" ))
  ("C-x" '(:ignore t :which-key "Miscellaneous" ))
  :hook ( dired-mode . dired-hide-details-mode)
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-alh1vG --group-directories-first"    ; human readable sizes
        dired-auto-revert-buffer t       ; revert buffer on revisit
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :config
  :ensure nil)

;; implement empty subdir collapsing
(use-package dired-collapse
  :hook (dired-mode  . dired-collapse-mode)
  :config)

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
            "<tab>" #'dired-subtree-toggle)
  :config)

;; edit dired buffers
(use-package wdired
  :init
  (setq wdired-allow-to-change-permissions t)
  :ensure nil
  :general
  (:keymaps 'dired-mode-map
            "<f5> w" #'wdired-change-to-wdired-mode)
  :config)

(use-package peep-dired
  :init
  ;; This makes peep-dired-mode-map override all evil mappings
  ;; evil-make-overriding-map doesn't handle j and k for some reason.
  (add-hook 'peep-dired-hook (lambda ()
                               (evil-make-intercept-map peep-dired-mode-map 'normal)
                               (evil-normalize-keymaps 'normal)))
  (setq peep-dired-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "j")         #'peep-dired-next-file)
          (define-key map (kbd "k")         #'peep-dired-prev-file)
          (define-key map (kbd "C-f")       #'peep-dired-scroll-page-down)
          (define-key map (kbd "C-b")       #'peep-dired-scroll-page-up)
          (define-key map (kbd "q")         #'peep-dired)
          map))
  :general
  (:keymaps 'dired-mode-map
            "<f5> p" #'peep-dired)
  :config)

(use-package dired-sidebar
  :custom
  (dired-sidebar-subtree-line-prefix "__")
  (dired-sidebar-theme 'nerd)
  (dired-sidebar-close-sidebar-on-file-open t)
  :general
  ("<f7> h"   #'dired-sidebar-toggle-sidebar)
  ("<f4> h"   #'dired-sidebar-toggle-with-current-directory)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))


(use-package flyspell
  :general
  ;; N.B. C-; is flyspell-correct-previous-word
  ("C-M-;"      #'flyspell-correct-previous-word-generic)
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
  :config
  (use-package flyspell-correct-ivy :demand))

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
  (advice-add #'define-word :before #'me:advise-define-word)
  :init)

(use-package ivy
  :general
  ("<f4> <f4>"  #'ivy-resume)
  ("<f2>"       #'ivy-switch-buffer)
  ("C-x b"      #'ivy-switch-buffer)
  (:keymaps 'ivy-minibuffer-map
            "<return>"  #'ivy-done
            "<C-return>" #'me:ivy-call-and-next-line
            "<C-S-return>" #'ivy-previous-line-and-call
            "<M-return>" #'ivy-dispatching-done
            "<C-right>" #'ivy-next-history-element
            "<C-left>" #'ivy-previous-history-element
            "<M-left>"  #'ivy-prev-action
            "<M-right>" #'ivy-next-action
            "<C-SPC>" #'me:ivy-toggle-mark-and-next-line
            "<M-SPC>" #'ivy-toggle-marks
            "M-y" #'ivy-next-line)                       ; for yank-pop flow
  ;; add some vim mappings in normal mode minibuffer
  (:keymaps 'ivy-minibuffer-map :states 'normal
            "C-b" #'ivy-scroll-down-command
            "C-f" #'ivy-scroll-up-command
            "G" #'ivy-end-of-buffer
            "gg" #'ivy-beginning-of-buffer
            "<up>" #'ivy-previous-line
            "<down>" #'ivy-next-line)
  ;;Bindings under C-c. These are for bindings which are less frequently
  ;;used and/or hard to remember.  Frequent use bindings are duplicated for discovery.
  (:keymaps 'ivy-minibuffer-map :prefix "C-c"
            ;; These are the same as ivy
            "C-o"   '(ivy-occur                         :which-key "Open occur buffer")
            "C-s"  '(ivy-rotate-sort                    :which-key "Rotate sorting method")
            ;; These have been moved so remove ivy's binding
            "C-a"   nil
            ;; Duplicates of ivy minibuffer bindings (old and new) for discoverability
            "C-r"     '(ivy-reverse-i-search            :which-key "Search history ⧉")
            "<C-left>"  '(ivy-previous-history-element  :which-key "Previous input history ⧉")
            "<C-right>" '(ivy-next-history-element      :which-key "Next input history ⧉")
            "<M-left>"  '(ivy-prev-action               :which-key "Previous action ⧉")
            "<M-right>" '(ivy-next-action               :which-key "Next action ⧉")
            "<return>" '(ivy-done                       :which-key "Call, exit ⧉")
            "<C-return>" '(me:ivy-call-and-next-line    :which-key "Call, Next line ⧉")
            "<C-S-return>" '(ivy-previous-line-and-call :which-key "Previous line, call ⧉")
            "<M-return>" '(ivy-dispatching-done         :which-key "Get action, call, exit ⧉")
            "<C-SPC>" '(me:ivy-toggle-mark-and-next-line  :which-key "Toggle mark, next line ⧉")
            "M-SPC" '(ivy-toggle-marks                  :which-key "Toggle marks ⧉" :override t)
            "M-j"     '(ivy-yank-word                   :which-key "Yank from buffer ⧉" :override t)
            ;; New bindings for this prefix only
            "t"  '(:ignore t :which-key "Toggles→" )
            "C-c"     '(ivy-avy                         :which-key "Avy search")
            "a"       '(ivy-read-action                 :which-key "Select default action")
            "n"       '(ivy-immediate-done              :which-key "Exit with input instead of candidate")
            "i"       '(ivy-insert-current              :which-key "Copy candidate to input")
            "r"       '(ivy-restrict-to-matches         :which-key "Rematch matched candidates")
            "w"       '(ivy-kill-ring-save              :which-key "Copy current candidates to kill ring")
            "?"       '(ivy-help                        :which-key "Ivy Help"))
  (:keymaps 'ivy-minibuffer-map :prefix "C-c t"
            "i"  '(ivy-toggle-ignore :which-key "Toggle ignore")
            "q"  '(ivy-toggle-regexp-quote :which-key "Toggle regex quoting")
            "c"  '(ivy-toggle-calling :which-key "Toggle calling")
            "f"  '(ivy-toggle-fuzzy :which-key "Toggle fuzzy")
            "u"  '(ivy-toggle-case-fold :which-key "Toggle case folding"))
  :init
  (setq ivy-use-virtual-buffers t                           ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-virtual-abbreviate 'full                        ; use full path for abbreviation
        ivy-count-format " (%-d) "                          ; show # candidates
        ivy-pre-prompt-function #'ivy-action-name           ; show action before # candidates
        ivy-initial-inputs-alist nil                        ; no regexp by default
        ivy-action-wrap t                                   ; wrap-around for actions
        ivy-on-del-error-function nil                       ; too many backspaces doesn't exit
        ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))                   ; allow input not in order
  :config

  ;; Make the default height be 1/3 of frame
  (defun me:ivy-height-from-frame (caller)
    (/ (+ 2 (frame-height)) 3))
  (add-to-list  'ivy-height-alist '( t . me:ivy-height-from-frame) t)


  (defun me:ivy-toggle-mark-and-next-line ()
    (interactive)
    (if (ivy--marked-p)
        (ivy-unmark)
      (ivy-mark)))

  (defun me:ivy-call-and-next-line ()
    (interactive)
    (ivy-call)
    (ivy-next-line))

  (defun me:ivy-dispatch-call-and-next-line ()
    (interactive)
    (ivy-dispatching-call)
    (ivy-next-line))

  (ivy-mode 1))

;; add some ivy buffer information
(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-format-function #'ivy-format-function-line)
  :config
  ;; make candidate column and mode bigger, rearrange columns from default
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     (
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                      (ivy-rich-candidate (:width 64))
                      (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
                      (ivy-rich-switch-buffer-project (:width 15 :face success))
                      (ivy-rich-switch-buffer-size (:width 7))
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
                      )
                     :predicate
                     (lambda (cand) (get-buffer cand)))))

  (ivy-rich-mode)
  :demand t)

(use-package counsel
  :defines counsel-yank-pop-preselect-last
  :commands (  counsel-file-jump counsel-find-file)
  :general
  (:keymaps 'global :prefix "<f4>"
            "a" #'counsel-apropos
            "b" #'counsel-mark-ring
            "D" #'counsel-dired-jump
            "g" #'counsel-search
            "i" #'counsel-info-lookup-symbol
            "j" #'counsel-bookmark
            "k" #'counsel-descbinds
            "l" #'counsel-linux-app
            "p" #'counsel-package
            "r" #'counsel-recentf
            "s"  '(:ignore t :which-key "Search→" )
            "u" #'counsel-unicode-char)
  (:keymaps 'global :prefix "<f4> s"
            "a" #'me:counsel-ag-here
            "l" #'counsel-locate
            "g" #'counsel-git-grep
            "s" #'me:counsel-rg-here
            "w" #'swiper
            "W" #'swiper-all)
  ("C-h b"  #'counsel-descbinds)
  ("M-x"    #'counsel-M-x)
  ("M-y"    #'counsel-yank-pop)
  ("M-<f2>" #'me:find-window-buffer)
  ("<f3>"   #'me:find-some-files)
  ("<f10> t" #'counsel-load-theme)
  ("<f10> c" #'counsel-colors-emacs)
  ("<f10> w" #'counsel-colors-web)
  ("<f6> m" #'counsel-imenu)
  :custom
  (counsel-search-engine 'google)
  (counsel-rg-base-command "rg --max-columns 500 --with-filename --no-heading --line-number --color never %s")
  :init
  (setq counsel-yank-pop-separator "\n---\n")
  :config
  (defun me:counsel-yank-pop-preselect-last ()
    (interactive)
    (let ((counsel-yank-pop-preselect-last t))
      (call-interactively (counsel-yank-pop))))
  (defun me:find-file (prompt candidates action caller)
    "Find a file from a list of files"
    (ivy-read prompt candidates
              :matcher #'counsel--find-file-matcher
              :action action
              :preselect (counsel--preselect-file)
              :require-match t
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller caller))
  (defun me:find-some-files ()
    "Find files in project or fallback to current directory"
    (interactive)
    (if (and (fboundp 'projectile-project-p) (projectile-project-p))
        (let* ((project-root (projectile-ensure-project (projectile-project-root)))
               (action (lambda (f) (with-ivy-window (find-file (expand-file-name f project-root)))))
               (files (projectile-project-files project-root)))
          (me:find-file "Find file: " files action 'me:find-some-files))
      (counsel-find-file)))
  (defun me:find-window-buffer()
    (interactive)
    "Find buffer from buffers previously used in window"
    (when-let* ((allbufs (mapcar 'car (window-prev-buffers)))
                (bufs (remove (current-buffer) allbufs))
                (cands (mapcar #'buffer-name bufs)))
      (ivy-read "Buffer: " cands
                :matcher #'ivy--switch-buffer-matcher
                :action #'ivy--switch-buffer-action
                :caller 'ivy-switch-buffer
                :history 'buffer-name-history
                :preselect (buffer-name (other-buffer (current-buffer)))
                :keymap ivy-switch-buffer-map)))
  (defun me:counsel-ag-here ()
    "Search using ag in default directory"
    (interactive)
    (counsel-ag (thing-at-point 'symbol) default-directory))
  (defun me:counsel-rg-here ()
    "Search using ripgrep in default directory"
    (interactive)
    (counsel-rg (thing-at-point 'symbol) default-directory))

  (counsel-mode 1))

;; Need this for counsel-search
(use-package request)

;; better M-x
(use-package amx
  :hook (( ivy-mode ) . amx-mode)
  :config
  :init)

;; allow grep buffers to be editted
(use-package wgrep
  :general
  (:keymaps 'grep-mode-map "<f5> w" #'wgrep-change-to-wgrep-mode))

(use-package projectile
  :after evil
  :demand t     ; required because use-package-always-defer is t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-globally-ignored-files #'( "TAGS" "GTAGS" "GRTAGS" "GPATH" ))
  (projectile-globally-ignored-file-suffixes #'( ".o" ".so" ".a" ".ko" ".jar" ".bc" ".class"))
  ;; we mainly want projects defined by a few markers and we always want to take the top-most marker.
  ;; Reorder so other cases are secondary
  (projectile-project-root-files #'( ".projectile" ))
  (projectile-project-root-files-functions #'(projectile-root-top-down
                                              projectile-root-bottom-up
                                              projectile-root-local))
  (projectile-use-git-grep t)
  (projectile-indexing-method 'hybrid)      ;; default indexing method is total crap
  (projectile-enable-caching t)
  :general
  (:keymaps 'projectile-mode-map "<f7>" 'projectile-command-map )  ; all projectile built in bindings off f7
  ("<f7> N"     #'projectile-clear-known-projects)
  ("<f7> n"     #'projectile-add-known-project)
  (:prefix "<f7>" "4"  '(:ignore t :which-key "Find→" ))
  (:prefix "<f7>" "5"  '(:ignore t :which-key "Find→" ))
  (:prefix "<f7>" "x"  '(:ignore t :which-key "Run→" ))
  (:prefix "<f7>" "s"  '(:ignore t :which-key "Search→" ))
  (:prefix "<f7> s" :keymaps 'projectile-mode-map
           "a" #'me:counsel-ag-project
           "r" nil  ;; unbind projectile-ripgrep
           "s" #'me:counsel-rg-project)
  ;; For reasons I don't understand, if projectile's map
  ;; has ESC in its map, this prevents the binding
  ;; <f7> <f7> for persp-projectile, but only in "emacs -nw" mode!
  ;; So getting rid of it here.
  (:prefix "<f7>" :keymaps 'projectile-mode-map
           "ESC" nil)

  :init
  (setq
   projectile-project-compilation-cmd "")     ;; workaround for stupid projectile bug
  :config
  (defun me:counsel-ag-project ()
    "Search using ag in project"
    (interactive)
    (if (and (fboundp 'projectile-project-p) (projectile-project-p))
        (counsel-ag (thing-at-point 'symbol) (projectile-project-root))
      (message "Not in a project")))
  (defun me:counsel-rg-project ()
    "Search using ripgrep in project"
    (interactive)
    (if (and (fboundp 'projectile-project-p) (projectile-project-p))
        (counsel-rg (thing-at-point 'symbol) (projectile-project-root))
      (message "Not in a project")))

  (defun me:add-project-templates ()
    (add-to-list 'org-capture-templates `("pn" "Project Notes" entry (file+headline ,(me:project-path "Notes/notes.org") "Notes")))
    (add-to-list 'org-capture-templates `("pd" "Project Tasks" entry (file+headline ,(me:project-path "Notes/notes.org") "TODOs")
                                          "* TODO %?\n  %i\n  %a")))
  (defun me:project-path ( &optional path)
    (if path
        (expand-file-name path (projectile-project-root))
      (projectile-project-root)))
  (projectile-mode 1))

(use-package persp-projectile
  :after projectile
  :demand
  :general
  (:prefix "<f7>" "<f7>"  #'projectile-persp-switch-project)
  :config)

(use-package perspective
  :after persp-projectile
  :init
  :general
  ("<f7> r"     #'persp-rename)
  ("M-<right>"  #'persp-next)
  ("M-<left>"   #'persp-prev)
  (:prefix "C-x"
           "x"  '(:ignore t :which-key "Perspective→" ))
  :config
  (persp-mode))

;; Highlight delimiters by depth
(use-package rainbow-delimiters
  :init
  (me:add-hook-with-delay 'prog-mode-hook 8 #'rainbow-delimiters-mode)
  :config)

;; color color strings
(use-package rainbow-mode
  :hook (( emacs-lisp-mode js-mode ) . rainbow-mode )
  :init
  :config)

;; Highlight cursor's surrounding parentheses
(use-package highlight-parentheses
  :init
  (me:add-hook-with-delay 'prog-mode-hook 8 #'highlight-parentheses-mode)
  :config)

(use-package dtrt-indent
  :hook (( prog-mode text-mode ) . dtrt-indent-mode)
  :init
  (setq dtrt-indent-run-after-smie t)
  :config)

(use-package aggressive-indent
  :hook (( emacs-lisp-mode ) . aggressive-indent-mode)
  :config)

(use-package markdown-mode
  :config
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package org
  :commands org-capture
  :general
  ("<f8> t" #'org-todo-list)
  ("<f8> a" #'org-agenda)
  ("<f8> <f8>" #'me:search-notes)
  ("<f8> c" #'me:org-capture)
  (:keymaps 'normal :prefix "SPC"
            "e" '(org-toggle-latex-fragment :which-key "Toggle latex fragment display"))
  (:prefix "C-c"
           "C-x"  '(:ignore t :which-key "Org→" ))
  :init
  (defconst me:command (expand-file-name "Notes/command.org" me:data-directory))
  (defconst me:language (expand-file-name "Notes/language.org" me:data-directory))
  (defconst me:system (expand-file-name "Notes/system.org" me:data-directory))
  (defconst me:android (expand-file-name "Notes/android.org" me:data-directory))
  (defconst me:home-notes (expand-file-name "Notes/notes.org" "~"))
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (add-hook 'org-babel-after-execute-hook #'me:redisplay-inline-images)
  (setq org-confirm-babel-evaluate #'me:babel-should-confirm
        org-plantuml-jar-path (expand-file-name "java/plantuml.jar" me:data-directory)
        org-ascii-bullets '((ascii 42) (latin1 167) (utf-8 8226))
        org-return-follows-link t
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
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (defun me:redisplay-inline-images ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))
  (defun me:babel-should-confirm (lang body)
    (not (member lang '( "plantuml" "ditaa" ))))
  (defun me:org-capture ()
    (interactive)
    (setq org-capture-templates me:org-capture-templates)
    (me:add-project-templates)
    (counsel-org-capture))
  (defun me:search-notes ()
    (interactive)
    (let* ((dot-notes (expand-file-name "Notes" me:data-directory))
           (proj-notes (me:project-path "Notes"))
           (home-notes (expand-file-name "Notes" "~"))
           (proj-notes-path (if (file-exists-p proj-notes) proj-notes ""))
           (home-notes-path (if (file-exists-p home-notes) home-notes "")))
      (counsel-rg (thing-at-point 'symbol) dot-notes (concat " -- " home-notes-path " " proj-notes-path " " dot-notes) nil))))

(use-package evil-org
  :after ( org evil )
  :hook (( org-mode ) . evil-org-mode)
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
                    tab-width 4)))
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

;; built-in restructured text mode
(use-package rst-mode
  :ensure nil
  :init
  (setq rst-pdf-program "xdg-open"))

(use-package sphinx-mode
  :hook (( rst-mode ) . sphinx-mode))

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

;; N.B. will need to run jedi:setup-server once
(use-package company-jedi
  :hook ((python-mode . me:setup-jedi))
  :init
  (setq jedi:complete-on-dot t)
  :config
  (defun me:setup-jedi()
    (jedi:setup)
    (make-local-variable 'company-backends)
    (delete 'company-capf company-backends)
    (add-to-list 'company-backends 'company-jedi)))

(use-package ruby-mode
  :config
  :mode
  (("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
   ("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package js2-mode
  :config
  :mode
  (("\\.js\\'" . js2-mode)
   ("\\.json\\'" . js2-mode)))

;; basic setup for java code
(use-package java-mode
  :ensure nil
  :init
  :config
  :mode
  (("\\.java\\'" . java-mode))
  (("\\.aidl\\'" . java-mode))                               ; Hack AIDL syntax highlighting
  (("\\.hal\\'" . java-mode)))                               ; Hack HIDL syntax highlighting

(use-package cc-mode
  :init
  (add-hook 'c++-mode-hook
            (lambda ()
              (define-key c++-mode-map ":" #'self-insert-command)
              (define-key c++-mode-map ")" #'self-insert-command)
              (define-key c++-mode-map ";" #'self-insert-command)
              (c-set-style "ambihelical" nil)
              (c-set-offset 'arglist-intro '+)               ; indent args extra
              (c-set-offset 'innamespace [0])))              ; no indentation in namespace
  (setq c-electric-pound-behavior (quote (alignleft))        ; cpp directives aligned to left
        show-paren-mode 0)
  :config
  ;; use ellemtel style but use c-basic-offset of 4
  (c-add-style "ambihelical" '("ellemtel" (c-basic-offset . 4)))

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

(use-package logview
  :general
  ("<f5> c" #'me:colorize-ansi-escapes)
  :config
  (defun me:colorize-ansi-escapes ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))
  :mode
  (("\\.log\\'"   . logview-mode)))

;; font lock for newer c++ versions
(use-package modern-cpp-font-lock
  :hook ( c++-mode . modern-c++-font-lock-mode )
  :config)

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
            "<f5> r" #'me:rotate-skip-threshold
            "<SPC>" nil
            "g" nil
            "j" nil
            "k" nil
            "h" nil
            "l" nil)
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
  :init
  (setq compilation-scroll-output t
        compilation-ask-about-save nil                 ; save all modified
        compilation-always-kill t                      ; always kill existing process
        compilation-auto-jump-to-first-error t
        compilation-finish-functions (lambda (buf str)
                                       (compilation-set-skip-threshold 1)
                                       (x-urgency-hint (selected-frame))
                                       (if (null (string-match ".*exited abnormally.*" str))
                                           ;;if no errors, make the compilation window go away in a few seconds
                                           (progn
                                             (run-at-time "2 sec" nil 'delete-windows-on buf)
                                             (message "No Compilation Errors!"))
                                         compilation-skip-threshold 2)))
  (add-hook 'compilation-start-hook
            (lambda (_proc) (compilation-set-skip-threshold 2)))

  (add-hook 'compilation-mode-hook (lambda ()
                                     (when (fboundp 'evil-make-intercept-map)
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
            "<f6> c"  #'eglot-code-actions
            "<f6> h"  #'eglot-help-at-point)
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :init
  (setq eglot-ignored-server-capabilites '( :documentHighlightProvider)
        eglot-send-changes-idle-time 3    ;; be slower sending changes
        eglot-events-buffer-size 100000)  ;; smaller events buffer
  :config
  ;; use clangd if present, otherwise assume ccls
  (if-let ((clangd (seq-find #'executable-find '("clangd" "clangd-6.0"))))
      (add-to-list 'eglot-server-programs
                   `((c++-mode c-mode) ,clangd))
    ;; use an absolute path for ccls cache
    ;; ccls uses unique cache directory name for each project so there are no collisions
    (let* ((cache-dir (expand-file-name "ccls-cache" me:cache-directory))
           (init-str (concat "--init={\"cache\":{\"directory\":\"" cache-dir "\"}}")))
      (add-to-list 'eglot-server-programs
                   `((c++-mode c-mode) "ccls" ,init-str))))

  ;; project-find-function which uses projectile methods to find
  ;; the projectile project associated with a directory.
  ;; If projectile not loaded, or directory is not in a project,
  ;; hopefully returns nil.
  (defun me:project-finder (dir)
    (if (boundp 'projectile-project-root-cache)
        (let ((root (projectile-project-root dir)))
          (and root (cons 'transient root)))))
  (add-to-list 'project-find-functions #'me:project-finder))


;; N.B. Completion when candidate is already typed out is broken in company.
;; See issues #451, #205, #150
(use-package company
  :general
  ("C-("     #'company-complete)  ; force completion
  ;; N.B. Workaround company disabling backend expansion of parameters in tng mode
  ;; Use this after tabbing to selection.  Normally any non-command key
  ;; will trigger the completion but suppress parameter expansion, this avoids
  ;; the suppression.
  (:keymaps 'company-active-map "(" #'company-complete-selection)
  :init
  (setq company-minimum-prefix-length 2            ; # chars needed for completion
        company-idle-delay 1
        company-dabbrev-downcase nil)              ; never downcase
  :config
  (company-tng-configure-default)
  :hook ( prog-mode . company-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :config
  (company-quickhelp-mode 1))

;; built-in package for cross-references
(use-package xref
  :demand
  :ensure nil
  :config
  (add-to-list 'xref-prompt-for-identifier #'xref-find-references t)
  :general
  ("<f6> <f6>" #'xref-find-definitions)
  ("<f6> d"    #'xref-find-definitions-other-window)
  ("<f6> r"   #'xref-find-references)
  ("<f6> a"   #'xref-find-apropos))

;; ivy interface to xref
(use-package ivy-xref
  :commands (ivy-xref-show-xrefs)
  :after xref
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs
        ivy-xref-use-file-path t))

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
  (define-key yas-minor-mode-map (kbd "TAB") nil))   ; don't use TAB

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-mode-line-prefix "☑"
        flycheck-idle-change-delay 3)     ;; default of 0.5s is too noisy
  (me:add-hook-with-delay 'prog-mode-hook 3 #'flycheck-mode)
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
  (me:add-hook-with-delay 'prog-mode-hook   5 #'hs-minor-mode))

(use-package avy
  :commands ( avy-goto-word-1 avy-goto-char-2 avy-goto-char-in-line )
  :config
  :init
  (setq avy-all-windows 'all-frames))

(use-package evil
  :hook (( prog-mode text-mode ) . evil-local-mode)

  :custom
  (evil-want-keybinding nil)  ; use evil-collection instead (needs to be done early)
  (evil-respect-visual-line-mode t)   ; movements respect visual line mode
  (evil-want-C-u-delete t)              ; use vim C-u binding (M-u replaces)
  (evil-want-C-u-scroll t)             ; use vim C-u binding (M-u replaces)
  ;; N.B. This needs to be after evil-want-C-u-delete to work
  (evil-disable-insert-state-bindings t) ; allow emacs bindings in insert mode

  :init
  ;; make cut/paste more vim-like
  ;; mainly keep emacs cut/paste separate from system clipboard
  ;; in the default case (must use "+ or "* to override)
  ;; This assumes select-enable-clipboard is set to nil as well
  (add-hook 'evil-local-mode-hook (lambda ()
                                    (setq-default interprogram-paste-function nil
                                                  interprogram-cut-function nil)))
  (setq-default evil-symbol-word-search t   ; misnamed: t is search for symbols, not words
                evil-shift-width tab-width)         ; shift by ideal width :)
  (setq evil-want-C-w-delete nil            ; want C-w for windows commands
        evil-want-C-w-in-emacs-state t      ; ditto
        evil-want-C-i-jump nil              ; need TAB for other things
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
            "<backtab>"   #'me:evil-shift-left-visual
            ;; Move visual selection up or down
            "<M-j>"       (concat ":m '>+1" (kbd "RET") "gv=gv")
            "<M-k>"       (concat ":m '<-2" (kbd "RET") "gv=gv"))
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
  (dolist (mode
           '(dired-mode
             finder-mode
             image-dired-thumbnail-mode
             cquery-tree-mode
             paradox-menu-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-mode 1))

(use-package evil-string-inflection
  :after evil
  :general
  (:states '(normal visual) :keymaps 'override
           :prefix "g"
           "-"          #'evil-operator-string-inflection)
  :init)

(use-package evil-collection
  :custom
  (evil-collection-setup-minibuffer t)
  :after evil
  :defer 1
  :config
  (evil-collection-init))

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
  :hook (( prog-mode text-mode ) . ws-butler-mode )
  :config)

(use-package shell-pop
  :general
  ("<f4> t"     #'shell-pop)
  :config
  (setq shell-pop-internal-mode "eshell"
        shell-pop-term-shell "/bin/bash"
        shell-pop-window-size 40
        shell-pop-window-position "bottom"
        shell-pop-universal-key "<f4> t"))

(use-package which-key
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-operator-state-maps t)
  :init
  :general
  ("<f5>" '(:ignore t :which-key "Major Mode Specific→" ))
  ("<f5> <f5>"  #'which-key-show-major-mode)
  :config
  (setq which-key-max-description-length 40
        which-key-side-window-max-width 0.67
        which-key-side-window-max-height 0.5
        which-key-sort-order 'which-key-local-then-key-order)
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  :defer 2
  :diminish which-key-mode)

(use-package with-editor
  :init
  (add-hook 'with-editor-mode-hook (lambda ()
                                     (evil-insert-state)
                                     (setq fill-column 70)))
  :config
  :diminish with-editor-mode)

(use-package magit
  :after evil
  :init
  (setq magit-completing-read-function 'ivy-completing-read   ; use ivy
        magit-save-repository-buffers 'dontask                ; save repo modified buffers w/o asking
        magit-section-initial-visibility-alist '(( stashes . hide ))
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-repository-directories '(( "~/dev" . 1)))
  :general
  ("<f9> a"     #'magit-commit-amend)
  ("<f9> b"     #'magit-blame)
  ("<f9> B"     #'magit-run-git-gui-blame)
  ("<f9> c"     #'magit-commit)
  ("<f9> f"     #'magit-log-buffer-file)
  ("<f9> l"     #'magit-log-current)
  ("<f9> o"     #'me:magit-open-revision)
  ("<f9> r"     #'magit-list-repositories)
  ("<f9> <f9>"  #'magit-status)
  :config
  ;; I want these for buffer&window switching
  (define-key magit-section-mode-map (kbd "M-1") nil)
  (define-key magit-section-mode-map (kbd "M-2") nil)
  (define-key magit-section-mode-map (kbd "M-3") nil)
  (define-key magit-section-mode-map (kbd "M-4") nil)
  (define-key magit-section-mode-map (kbd "M-5") nil)
  (define-key magit-status-mode-map (kbd "M-w") nil)
  (defun me:magit-open-revision (rev arg)
    "Select and open revision of current file, with prefix opens in other window"
    (interactive (list (magit-read-branch-or-commit "Open revision") current-prefix-arg))
    (if arg
        (magit-find-file-other-window rev (buffer-file-name))
      (magit-find-file rev (buffer-file-name)))))

(use-package evil-magit
  :after magit
  :demand t
  :init
  :config
  (evil-define-key* evil-magit-state magit-mode-map [escape] nil))

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

(use-package popwin
  :init
  :config
  (push '("*Async Shell Command*" :noselect t) popwin:special-display-config)
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)
  (push '("*undo-tree*" :stick t :width 60 :position right) popwin:special-display-config)
  (push '("*General Keybindings*" :width 120 :position right) popwin:special-display-config)
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
