(let ((file-name-handler-alist nil))
(setq gc-cons-threshold 100000000)

;; package management

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/"))
      use-package-always-ensure t                              ; ensure by default
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

;; select the nth other buffer
(defun me:select-nth-other-buffer (arg)
  (interactive "p")
  (let ((buffer (nth arg (-filter 'buffer-file-name (buffer-list)))))
    (if buffer
        (switch-to-buffer buffer))))

;; rotate the current fill-column
(defun me:rotate-fill-column ()
  (interactive)
  (setq fill-column (cond ((< fill-column 120) (+ fill-column 10))
                          ((>= fill-column 120) 70)))
  (whitespace-mode -1)
  (whitespace-mode))

;; configuration I haven't figured out how to wedge into
;; use-package

(setq-default tab-width 3                                   ; ideal tab width
              indent-tabs-mode t                            ; enable tabs for most files
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
      save-interprogram-paste-before-kill t                 ; save clipboard before killing
      x-select-enable-clipboard nil                         ; make cut/paste function correctly
      split-width-threshold 240                             ; 2x ideal line width :)
      indicate-empty-lines t                                ; show empty lines at end of buffer
      visual-line-fringe-indicators '(left-curly-arrow nil) ; use left curly error for wrapped lines
      view-read-only t                                      ; show r/o files in view mode
      standard-indent 3                                     ; ideal indent :)
      sentence-end-double-space nil)                        ; sentences end with one space
(add-hook 'focus-out-hook #'me:save-dirty-buffers)          ; save on defocus
(add-hook 'mouse-leave-buffer-hook #'me:kill-minibuffer)    ; kill minibuffer on click outside
(add-hook 'after-init-hook                                  ; report init time
          (lambda ()
            (message "Time to initialize: %s"
                     (emacs-init-time))))
(add-hook #'prog-mode-hook                                  ; keyword highlighting
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|BUG\\|WARN\\|HACK\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(TODO\\|TBD\\):" 1 font-lock-keyword-face t)))))
(set-frame-font "Fantasque Sans Mono 12" t t)               ; nice font
(tool-bar-mode 0)                                           ; no tool bar
(scroll-bar-mode 0)                                         ; no scroll bar
(menu-bar-mode 0)                                           ; no menu bar
(mouse-avoidance-mode 'animate)                             ; move mouse pointer out of way
(column-number-mode t)                                      ; display column/row of cursor in mode-line
(global-hl-line-mode t)                                     ; highlight current line
(fset 'yes-or-no-p 'y-or-n-p)                               ; change stupid default y/n? y
(make-directory me:emacs-backup-directory t)                ; make sure backup dir exists
(global-auto-revert-mode t)                                 ; revert unchanged files automatically
(electric-indent-mode +1)                                   ; turn on electric mode globally
(global-visual-line-mode t)                                 ; wrap long lines
(winner-mode t)                                             ; enable winner mode
(run-at-time "1 hour" 3600 #'clean-buffer-list)             ; clear out old buffers every hour

(global-unset-key (kbd "<f4>"))
(global-unset-key (kbd "<f10>"))                            ; was menu-bar-open
(global-unset-key (kbd "<f11>"))                            ; was fullscreen mode
(global-set-key (kbd "s-1") #'helm-projectile-find-other-file)
(global-set-key (kbd "s-#") 'me:select-nth-other-buffer)    ; base mapping for the following...
(global-set-key (kbd "s-2") (kbd "C-u 1 s-#"))
(global-set-key (kbd "s-3") (kbd "C-u 2 s-#"))
(global-set-key (kbd "s-4") (kbd "C-u 3 s-#"))
(global-set-key (kbd "s-5") (kbd "C-u 4 s-#"))
(global-set-key (kbd "s-c") #'me:rotate-fill-column)
(global-set-key (kbd "s-]") #'winner-redo)
(global-set-key (kbd "s-[") #'winner-undo)
(global-set-key (kbd "s-;") #'rtags-location-stack-forward)
(global-set-key (kbd "s-,") #'rtags-location-stack-back)
(global-set-key (kbd "s-`") #'previous-buffer)
(global-set-key (kbd "<s-tab>") #'next-buffer)
(global-set-key (kbd "<f5> h") #'global-hl-line-mode)
(global-set-key (kbd "<f5> m") #'menu-bar-mode)
(global-set-key (kbd "<f5> <f5>") #'menu-bar-open)
(global-set-key (kbd "<f5> f") #'toggle-frame-fullscreen)

;; elisp mode settings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 2
                  standard-indent 2
                  indent-tabs-mode nil                      ; no tabs
                  evil-shift-width 2                        ; need this since no tabs
                  lisp-body-indent 2)))                     ; indent elisp by 2

(use-package diminish
  :init
  ;; some of these need to be run after init file has loaded
  ;; to actually be diminished
  (add-hook 'after-init-hook (lambda ()
                               (diminish 'visual-line-mode)
                               (diminish 'abbrev-mode)
                               (diminish 'auto-revert-mode))))

(use-package dash-functional)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t)
  :defer 3
  :init
  (setq smooth-scroll-margin 5
        smooth-scroll-strict-margins t))

(use-package whitespace
  :commands whitespace-mode
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
  :defer 3
  :config
  (beacon-mode 1)
  :diminish beacon-mode)

(use-package adaptive-wrap
  :init
  (setq-default adaptive-wrap-extra-indent 3)
  :config
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  :defer 4)

(use-package leuven-theme
  :init
  (add-hook 'whitespace-mode-hook (lambda ()
    (set-face-attribute 'whitespace-line nil :foreground 'unspecified :background "lemon chiffon")
    (set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background "white" )
    (set-face-attribute 'whitespace-trailing nil :foreground "black" :background "red" )))
  :config
  (set-face-attribute 'hl-line nil :foreground 'unspecified :background "gainsboro"))

(use-package spaceline-config
  :ensure spaceline
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator " ")
  :config
  (progn
    (use-package powerline
      :init
      (setq powerline-default-separator 'wave))
    (spaceline-spacemacs-theme)))

(use-package diff-hl
  :init
  (progn
    (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  :config
  (progn
    (when (not (display-graphic-p))
      (diff-hl-margin-mode 1))
    (diff-hl-flydiff-mode 1)       ; needs 24.4 or newer
    (global-diff-hl-mode 1))
  :defer 4)

(use-package linum-relative
  :diminish linum-relative-mode
  :init
  (setq linum-relative-current-symbol "")   ; show current line #
  :bind
  (("<f5> l" . linum-relative-mode)))

(use-package ruler-mode
  :bind
  (("<f5> r" . ruler-mode)))

;; better package manager
(use-package paradox
  :commands paradox-list-packages
  :init
  (setq paradox-spinner-type 'moon
        paradox-execute-asynchronously nil
        paradox-display-download-count t
        paradox-display-star-count t
        paradox-github-token t                ; Dont't ask, don't integrate
        paradox-automatically-star nil        ; Don't star automatically
        paradox-hide-wiki-packages t)
  :bind
  (("<f4> p" . paradox-list-packages)))

(use-package recentf
  :config (recentf-mode)
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300))           ; wait 5m before 1st cleanup

(use-package undo-tree
  :demand
  :init (global-undo-tree-mode)
  :bind
  (("<f4> v" . undo-tree-visualize ))
  :diminish undo-tree-mode)

(use-package dired
  :ensure nil
  :init
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t)             ; use existing dired buffer, if exists
  :config
  (progn
    (use-package peep-dired
      :bind (:map dired-mode-map
              ("C-f" . peep-dired)))
    (use-package dired-narrow
      :bind (:map dired-mode-map
                  ("/" . dired-narrow)))
    (use-package dired+
      :defer 2
      :init
      (setq font-lock-maximum-decoration (quote ((dired-mode . nil) (t . t)))   ; turn off barf colors
            diredp-hide-details-initially_flag t
            diredp-image-preview-in-tooltip 400
            diredp-auto-focus-frame-for-thumbnail-tooltip-flag t)))
  :bind
  (("<f4> d" . dired-jump))
  :defer t)

(use-package flyspell
  :commands ( flyspell-prog-mode flyspell-mode )
  :init
  (progn
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (add-hook 'text-mode-hook #'flyspell-mode))
  :config
  (progn
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
  :bind
  (("s-f" . flyspell-auto-correct-previous-word))
  :diminish (flyspell-mode . "Ⓢ"))

  (use-package helm-flyspell
    :bind
    (("s-s" . helm-flyspell-correct)))

(use-package helm
  :commands helm-semantic-or-imenu
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
        helm-ff-skip-boring-files             t            ; don't show boring files
        ;; set the sources for helm-for-files
        helm-for-files-preferred-list '( helm-source-recentf
                                         helm-source-buffers-list
                                         helm-source-files-in-current-dir
                                         helm-source-locate ))
  :config
  (progn
    (use-package helm-descbinds             ; Describe key bindings with Helm
      :defer 3
      :config
      (helm-descbinds-mode))
    (use-package helm-unicode
      :commands helm-unicode)
    (helm-autoresize-mode t)
    ;; swap tab/c-z as recommended by tuhdo
    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action) ; rebind tab to do persistent action
    (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  #'helm-select-action) ; list actions using C-z
    (define-key helm-map [escape] #'helm-keyboard-quit))

  :bind
  (("M-x"       . helm-M-x)
   ("C-x b"     . helm-mini)
   ("<f2>"      . helm-mini)
   ("<f3>"      . helm-for-files)
   ("<f4> a"    . helm-apropos)
   ("<f4> b"    . helm-all-mark-rings)
   ("<f4> f"    . helm-grep-do-git-grep)
   ("<f4> i"    . helm-info-at-point)
   ("<f4> j"    . helm-bookmarks)
   ("<f4> k"    . helm-descbinds)
   ("<f4> l"    . helm-locate)
   ("<f4> m"    . helm-man-woman)
   ("<f4> r"    . helm-recentf)
   ("<f4> u"    . helm-unicode)
   ("<f4> x"    . helm-top)
   ("<f4> y"    . helm-show-kill-ring)
   ("<f4> <f4>" . helm-resume)))

(use-package projectile
  :commands ( projectile-compile-project projectile-switch-project )
  :diminish projectile-mode
  :after evil
  :init
  (setq projectile-completion-system 'helm
        projectile-use-git-grep t
        projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))
        projectile-enable-caching nil)
  :config
  (progn
    (use-package helm-projectile
      :config
      (progn
        (helm-projectile-on)
        (setq projectile-switch-project-action 'helm-for-files)))
    (use-package persp-projectile)
    (use-package ag)
    (use-package helm-ag)
    (use-package grep)
    (setq helm-for-files-preferred-list '( helm-source-projectile-files-list
                                           helm-source-projectile-recentf-list
                                           helm-source-recentf
                                           helm-source-buffers-list
                                           helm-source-projectile-projects
                                           helm-source-files-in-current-dir
                                           helm-source-locate))
    (projectile-global-mode 1))
  :bind
  (("<f7> c"    . projectile-compile-project)
   ("<f7> <f7>" . projectile-persp-switch-project)
   ("<f7> o"    . projectile-multi-occur)
   ("<f7> u"    . projectile-invalidate-cache)
   ("<f7> k"    . projectile-kill-buffers)
   ("<f7> f"    . helm-projectile-ag)))

(use-package perspective
  :after projectile
  :config
  (progn
    (setq persp-initial-frame-name (projectile-project-name))
    (persp-mode))
  :bind
  (("<f7> r" . persp-rename)
   ("s-<right>" . persp-next)
   ("s-<left>" . persp-prev)))

(use-package smart-tabs-mode
  :defer 3
  :config
  (progn
    (smart-tabs-insinuate 'python)
    (smart-tabs-insinuate 'c 'c++)))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(use-package cmake-mode
  :init
  (progn
    (setq cmake-tab-width 3))
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

;; n.b. buffer-face-mode screws up completion popups
;; may be fixed in 25.1 or so
(use-package adoc-mode
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
  :mode
  (("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
   ("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package js2-mode
  :mode
  (("\\.js\\'" . js-mode)
   ("\\.json\\'" . js-mode)))

(use-package cc-mode
  :init
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (modify-syntax-entry ?_ "w")                   ; underscores are parts of words
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
  :init
  (progn
    (defun me:rotate-skip-threshold ()
      (interactive)
      (compilation-set-skip-threshold
       (cond ((= compilation-skip-threshold 1) 2)
             ((= compilation-skip-threshold 2) 0)
             (t 1))))
    (setq compilation-scroll-output t
          compilation-auto-jump-to-first-error t
          compilation-skip-threshold 2
          compilation-ask-about-save nil    ; save all modified
          compilation-finish-functions
          (lambda (buf str)
            (compilation-set-skip-threshold 1)
            (if (null (string-match ".*exited abnormally.*" str))
                ;;if no errors, make the compilation window go away in a few seconds
                ;;if errors, make it full sized
                (progn
                  (run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create "*compilation*"))
                  (message "No Compilation Errors!"))
              (delete-other-windows (get-buffer-window "*compilation*")))))
    (add-hook 'compilation-start-hook
              (lambda (proc) (compilation-set-skip-threshold 2)))
    (add-hook 'compilation-mode-hook
      (lambda ()
        (local-set-key (kbd "<tab>") #'me:rotate-skip-threshold)
        (local-set-key (kbd "f") #'next-error-follow-minor-mode)
        (local-set-key (kbd "k") #'compilation-previous-error)
        (local-set-key (kbd "j") #'compilation-next-error)
        (local-set-key (kbd "C-k") #'compilation-previous-file)
        (local-set-key (kbd "C-j") #'compilation-next-file)
        (local-set-key (kbd "<prior>") #'compilation-previous-error)
        (local-set-key (kbd "<next>") #'compilation-next-error)
        (local-set-key (kbd "<home>") #'compilation-previous-file)
        (local-set-key (kbd "<end>") #'compilation-next-file)))))

(use-package deft
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
                (define-key deft-mode-map (kbd "<C-backspace>") #'deft-filter-clear))))
  :bind
  ("<f4> n" . deft))

(use-package company
  :commands ( global-company-mode company-complete )
  :init
  (setq company-minimum-prefix-length 1)
  :bind
  ("s-d"   . company-complete)
  :config
  (progn
    (use-package company-quickhelp
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
  :commands ( rtags-location-stack-back rtags-location-stack-forward )
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
    (rtags-enable-standard-keybindings))
  :bind
  (("<f6> <f6>" . me:tags-find-symbol-at-point)
   ("<f6> d"    . me:tags-find-symbol)
   ("<f6> r"    . me:tags-find-references-at-point)
   ("<f6> f"    . me:tags-find-file)
   ("<f6> v"    . rtags-find-virtuals-at-point)
   ("<f6> c"    . rtags-rename-symbol)
   ("<f6> i"    . rtags-find-functions-called-by-this-function)
   ("<f6> m"    . helm-semantic-or-imenu)
   ))

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
  :init
  (setq avy-all-windows 'all-frames))

;; N.B. evil-mode must be enabled after global-evil-leader-mode
(use-package evil
  :defer 1
  :init
  (progn
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
      :defer 3)
    (use-package evil-textobj-anyblock
      :defer 3)
    (use-package evil-commentary
      :defer 3
      :config (evil-commentary-mode)
      :diminish evil-commentary-mode)
    (use-package evil-surround
      :defer 3
      :config (global-evil-surround-mode 1))
    (use-package evil-leader
      :init
      (progn
        (setq evil-leader/in-all-states 1))
      :config
      (progn
        (global-evil-leader-mode t)
        (evil-leader/set-leader "<SPC>")
        (evil-leader/set-key
          "<SPC>" #'me:switch-to-previous-buffer
          ";" #'evil-jump-forward
          "," #'evil-jump-backward
          "a" #'align
          "c" #'me:use-evil-clipboard-register
          "e" #'pp-eval-last-sexp
          "f" #'evil-avy-goto-word-1
          "g" #'evil-avy-goto-char-2
          "l" #'evil-avy-goto-char-in-line
          "m" #'projectile-compile-project
          "o" #'me:switch-to-compile-buffer
          "r" #'recompile
          "s" #'me:use-evil-selection-register
          "v" #'exchange-point-and-mark
          "w" #'save-buffer
          "x" #'kill-buffer)))

    (evil-set-initial-state 'git-rebase-mode 'emacs)
    (evil-set-initial-state 'deft-mode 'insert)
    (evil-set-initial-state 'magit-branch-manager-mode 'emacs)
    (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
    (evil-set-initial-state 'rtags-mode 'emacs)
    (evil-set-initial-state 'dired-mode 'emacs)
    (evil-set-initial-state 'image-mode 'emacs)
    (evil-set-initial-state 'finder-mode 'emacs)
    (evil-set-initial-state 'image-dired-thumbnail-mode 'emacs)
    (evil-set-initial-state 'paradox-menu-mode 'emacs)

    ;; note evil-set-initial-state didn't work for these modes
    ;; I'm not sure why...
    (add-hook 'with-editor-mode-hook #'evil-insert-state)
    (add-hook 'magit-log-mode-hook #'evil-emacs-state)
    (add-hook 'magit-revision-mode-hook #'evil-emacs-state)
    ;;
    (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" #'evil-outer-arg)
    (define-key evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
    (define-key evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
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
  (ws-butler-global-mode t)
  :diminish ws-butler-mode
  :defer 3)

(use-package shell-pop
  :init
  (setq shell-pop-internal-mode "ansi-term"
        shell-pop-term-shell "/bin/bash"
        shell-pop-window-size 40
        shell-pop-window-position "top"
        shell-pop-universal-key "<f4> t")
  :bind
  ("<f4> t" . shell-pop))

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
  (add-hook 'with-editor-mode-hook (lambda () (setq fill-column 70)))
  :config
  (progn
    (use-package evil-magit
      :init
      (setq evil-magit-state 'normal)))
  :bind
  (("<f7> g" . magit-status)
   ("<f7> b" . magit-blame)
   ("<f7> a" . magit-run-git-gui-blame)))

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
    (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)
    (push '("*undo-tree*" :stick t :width 60 :position right) popwin:special-display-config)
    (popwin-mode 1))
  :defer 2)

;; Load system-dependent init file if it exists
;; will be in emacs.d/init-<prefix>-<ident>.el
(defun me:load-init-file (prefix ident)
  (let ((file-name (expand-file-name (concat "init-" prefix "-" (me:replace-all ident "/" "-") ".el") user-emacs-directory)))
  (message "looking for %s" file-name)
  (when (file-exists-p file-name)
    (message "loading %s" file-name)
    (load-file file-name))))

(me:load-init-file "system" (symbol-name system-type))
(me:load-init-file "host" system-name)

;; restore a normal gc threshold
(setq gc-cons-threshold 1000000))
