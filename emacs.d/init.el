
(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package ))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(setq use-package-always-ensure t
      use-package-minimum-reported-time 0.03
      use-package-verbose t)

;; configure the chrome
(set-default-font "DejaVu Sans Mono 9")
(cond
 ((string-equal system-type "darwin")
  (set-default-font "Menlo Regular 12")))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode t)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(setq gdb-many-windows t)                                   ; use more complex gdb layout
(mouse-avoidance-mode 'animate)                             ; move mouse pointer out of way
(column-number-mode t)                                      ; display column/row of cursor in mode-line
(display-time-mode t)                                       ; display time in mode-line

(defun me:replace-prefix (prefix input)
  (replace-regexp-in-string ( concat "^" (regexp-quote prefix)) "" input))

(setq frame-title-format '((:eval (if (buffer-file-name)
                                      (me:replace-prefix (abbreviate-file-name default-directory)
                                                         (abbreviate-file-name buffer-file-name))
                                    "%b"))
                           " %* ["
                           (:eval (abbreviate-file-name default-directory))
                           "]")
      icon-title-format frame-title-format)                 ; use same title for unselected frame

;; tame scrolling
(setq scroll-margin 5                                       ; leave 5 lines at top/bottom
      scroll-conservatively 100                             ; scroll # to bring point in view
      scroll-preserve-screen-position 'always               ; move cursor when scrolling
      mouse-wheel-scroll-amount '(3 ((shift) . 9))          ; 3 lines, or 9 line when shift held
      mouse-wheel-follow-mouse 't                           ; scroll window under mouse
      mouse-wheel-progressive-speed nil)                    ; don't speed up
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Operational preferences
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.cache/emacs")))
(setq custom-file "~/.cache/emacs/customize")               ; put customizations here
(global-auto-revert-mode t)                                 ; revert unchanged files automatically

; make some keys available for use
(global-unset-key (kbd "<f4>"))


;; Text handling
(global-visual-line-mode t)                                        ; edit visual lines
(setq visual-line-fringe-indicators '(left-curly-arrow nil))

(setq-default tab-width 3                                   ; ideal tab width
              indent-tabs-mode t                            ; enable tabs for most files
              fill-column 120)                              ; auto-wrap only very long lines
(setq standard-indent 3                                     ; ideal indent :)
      x-select-enable-clipboard nil                         ; make cut/paste function correctly
      sentence-end-double-space nil)                        ; sentences end with one space
(add-hook 'focus-out-hook
          (lambda ()
            (interactive)
            (save-some-buffers t)))         ; save on focus lost
(electric-indent-mode +1)                                   ; turn on electric mode globally

;; Align with spaces only
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(defadvice align (around align-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align)

; elisp mode settings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 2
                  standard-indent 2
                  indent-tabs-mode nil                      ; no tabs
                  evil-shift-width 2                        ; need this since no tabs
                  lisp-body-indent 2)))                     ; indent elisp by 2


(use-package whitespace
  :init
  (progn
    (setq whitespace-line-column 80                      ; highlight columns past 80
          whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab)
          whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
    (add-hook 'prog-mode-hook 'whitespace-mode))
  :diminish whitespace-mode)

(use-package fill-column-indicator
  :disabled
  :config
  (progn
    (add-hook 'after-change-major-mode-hook (lambda () (if buffer-file-name (fci-mode 1))))
    (setq fci-rule-color "white smoke")))

(use-package adaptive-wrap
  :init
  (progn
    (setq-default adaptive-wrap-extra-indent 3))
  :config
  (progn
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)))

(use-package leuven-theme
  :config
  (progn
    (set-face-attribute 'whitespace-line nil :foreground 'unspecified :background "lemon chiffon")
    (set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background "white" )
    (set-face-attribute 'whitespace-trailing nil :foreground "black" :background "red" )))

(use-package flyspell
  :config
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode))
  :diminish flyspell-mode
  :defer 4)

(use-package helm
  :init
  (progn
    (setq helm-split-window-in-side-p           t
          helm-split-window-default-side        'other
          helm-move-to-line-cycle-in-source     t
          helm-quick-update                     nil          ; ui flashing occurs
          helm-input-idle-delay                 0.1
          helm-idle-delay                       0.0
          helm-candidate-number-limit           100
          helm-scroll-amount                    8
          ;; set the sources for helm-for-files
          helm-for-files-preferred-list '( helm-source-recentf
                                           helm-source-buffers-list
                                           helm-source-files-in-current-dir
                                           helm-source-locate )))
  :config
  (progn
    (define-key helm-map [escape] 'helm-keyboard-quit))

  :bind
  (("M-x"       . helm-M-x)
   ("C-x b"     . helm-mini)
   ("<f2>"      . helm-mini)
   ("<f3>"      . helm-for-files)
   ("<S-f3>"    . dired-jump)
   ("<f4> a"    . helm-apropos)
   ("<f4> i"    . helm-info-at-point)
   ("<f4> j"    . helm-all-mark-rings)
   ("<f4> k"    . helm-show-kill-ring)
   ("<f4> l"    . helm-locate)
   ("<f4> m"    . helm-man-woman)
   ("<f4> p"    . helm-list-elisp-packages-no-fetch)
   ("<f4> x"    . helm-top)
   ("<f4> <f4>" . helm-resume))
  :defer 2)

(use-package projectile
  :init
  (progn
    (setq projectile-completion-system 'helm
          projectile-use-git-grep t
          projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))
          projectile-enable-caching t))
  :config
  (progn
    (use-package helm-projectile
      :config
      (progn
        (helm-projectile-on)
        (setq projectile-switch-project-action 'helm-for-files)))
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
  (("<f5>"      . helm-projectile-find-other-file)
   ("<f7> <f7>" . helm-projectile-switch-project)
   ("<f7> c"    . projectile-compile-project)
   ("<f7> o"    . projectile-multi-occur)
   ("<f7> u"    . projectile-invalidate-cache)
   ("<f7> k"    . projectile-kill-buffers)
   ("<f7> f"    . helm-projectile-ag)))

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-ignore-case t))
  :config
  (progn
    (defun me:update-all-tags ()
      (interactive)
      (let ((current-prefix-arg 4)) (call-interactively 'helm-gtags-update-tags)))
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode))
  :bind
  (("<f6> <f6>" . helm-gtags-dwim)
   ("<f6> d"    . helm-gtags-find-tag)
   ("<f6> r"    . helm-gtags-find-rtag)
   ("<f6> u"    . me:update-all-tags )
   ("<f6> m"    . helm-semantic-or-imenu))
  :diminish helm-gtags-mode)

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

; n.b. buffer-face-mode screws up completion popups
; may be fixed in 25.1 or so
(use-package adoc-mode
  :mode
  (("\\.ad\\'" . adoc-mode)
   ("\\.adoc\\'" . adoc-mode)
   ("\\.asciidoc\\'" . adoc-mode)))

(use-package smart-tabs-mode
  :config
  (progn
    (smart-tabs-insinuate 'python)
    (smart-tabs-insinuate 'c 'c++)))

(use-package cc-mode
  :init
  (progn
    (setq c-default-style "ellemtel"                           ; similar to allman style
          c-electric-pound-behavior (quote (alignleft))        ; cpp directives aligned to left
          show-paren-mode 0))

  :config
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (setq c-basic-offset 3)))
    (add-hook 'c++-mode-hook
              (lambda ()
                (c-set-offset 'innamespace [0]))))                       ; no indentation in namespace
  :mode
  (("\\.c\\'"   . c-mode)
   ("\\.cpp\\'" . c++-mode)
   ("\\.cxx\\'" . c++-mode)
   ("\\.h\\'"   . c++-mode)
   ("\\.hpp\\'" . c++-mode)))

(use-package compile
  :init
  (progn
    (setq compilation-scroll-output t)
    (add-hook 'compilation-mode-hook
      (lambda ()
        (next-error-follow-minor-mode t)
        (local-set-key (kbd "k") 'compilation-previous-error)
        (local-set-key (kbd "j") 'compilation-next-error)
        (local-set-key (kbd "C-k") 'compilation-previous-file)
        (local-set-key (kbd "C-j") 'compilation-next-file)
        (local-set-key (kbd "<prior>") 'compilation-previous-error)
        (local-set-key (kbd "<next>") 'compilation-next-error)
        (local-set-key (kbd "<home>") 'compilation-previous-file)
        (local-set-key (kbd "<end>") 'compilation-next-file))))
  :defer 3)

(use-package python-mode
  :init
  (progn
    (add-hook 'python-mode-hook
      (lambda ()
        (semantic-mode t)
        (setq evil-shift-width 4)
        (setq python-indent-offset 4)
        (setq python-indent-guess-indent-offset t)
        (setq tab-width 4))))
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.py3\\'" . python-mode)))

(use-package git-gutter
  :config
  (progn
    (global-git-gutter-mode t))
  :defer 3
  :diminish git-gutter-mode)

(use-package deft
  :init
  (progn
    (setq deft-directory "~/Dropbox/Notes"
          deft-recursive t
          deft-use-filter-string-for-filename t
          deft-file-naming-rules '((nospace . "_")
                                   (noslash . "_")
                                   (case-fn . downcase))
          deft-text-mode 'markdown-mode
          ; first extension in list seems to be used for new files.  Not sure
          ; what deft-default-extesion does
          deft-default-extension "md"
          deft-extensions '("md" "txt" "text" "markdown" "mmd" "org")
                                        ; deft auto-save interferes with whitespace-butler, so disable
          deft-auto-save-interval 0))


  :config
  (progn
    (add-hook 'deft-mode-hook
              (lambda ()
                (define-key deft-mode-map (kbd "<f4> n") 'quit-window)
                (define-key deft-mode-map (kbd "<C-return>") 'deft-new-file)
                (define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-clear))))
  :bind
  ("<f4> n" . deft))

(use-package company
  :init
  (progn
    (setq company-minimum-prefix-length 1)
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    ;(use-package helm-company :defer 3)
    (use-package company-irony
      :config
      (progn
        (add-to-list 'company-backends 'company-irony))))
  :diminish company-mode)

; N.B. to use, need to run irony-install-server, which requires libclang-dev
(use-package irony
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun me:irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'me:irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
  :config
  (progn
    (use-package flycheck-irony
      :config
      (progn
        (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))
  :diminish irony-mode)


(use-package flycheck
  :config
  (progn
    (global-flycheck-mode))
  :defer 4)

; enable code folding (evil has bindings)
(use-package hideshow
  :init
  (progn
    (add-hook 'c-mode-common-hook   'hs-minor-mode)
    (add-hook 'c++-mode-hook        'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
    (add-hook 'sh-mode-hook         'hs-minor-mode)
    (add-hook 'python-mode-hook     'hs-minor-mode))
  :diminish hs-minor-mode)

(use-package avy :defer 3)

;; N.B. evil-mode must be enabled after global-evil-leader-mode
(use-package evil
  :init
  (progn
    (setq-default evil-symbol-word-search t
                  evil-shift-width 3)
    (setq evil-search-module 'evil-search))
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
    (use-package evil-args)
    (use-package evil-textobj-anyblock)
    (use-package evil-commentary
      :config (evil-commentary-mode)
      :diminish evil-commentary-mode)
    ; evil-snipe, but change keybindings to be a text-object motion  (i.e. op [ia] [zZ] cc)
    ; to make it easier to remember.  Also, disable normal-mode s/S override, and
    ; use leader-[zZ] instead.
    (use-package evil-snipe
      :init
      (progn
        (setq evil-snipe-auto-disable-substitute nil))          ; disable override of s operator
      :diminish evil-snipe-local-mode
      :config
      (progn
        (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
        (define-key evil-inner-text-objects-map "z" 'evil-snipe-x)
        (define-key evil-outer-text-objects-map "z" 'evil-snipe-s)
        (define-key evil-inner-text-objects-map "Z" 'evil-snipe-X)
        (define-key evil-outer-text-objects-map "Z" 'evil-snipe-S)
        (evil-snipe-override-mode 1)
        (evil-snipe-mode 1)))
    (use-package evil-leader
      :init
      (progn
        (setq evil-leader/in-all-states 1))
      :config
      (progn
        (global-evil-leader-mode t)
        (evil-leader/set-leader "<SPC>")
        (add-hook 'projectile-mode-hook (lambda () (evil-leader/set-key "m" 'projectile-compile-project)))
        (evil-leader/set-key
          ";" 'evil-jump-forward
          "," 'evil-jump-backward
          "a" 'align
          "c" 'me:use-evil-clipboard-register
          "e" 'pp-eval-last-sexp
          "f" 'avy-goto-char-2
          "s" 'me:use-evil-selection-register
          "v"  'exchange-point-and-mark
          "w" 'save-buffer
          "x" 'kill-buffer
          "z" 'evil-snipe-s
          "Z" 'evil-snipe-S)))
    (use-package powerline-evil
      :config
      (progn
        (powerline-default-theme)
        (display-time-mode t)))

    (evil-set-initial-state 'git-rebase-mode 'emacs)
    (evil-set-initial-state 'deft-mode 'insert)
    (evil-set-initial-state 'magit-branch-manager-mode 'emacs)
    ; note evil-set-initial-state didn't work for this mode
    (add-hook 'with-editor-mode-hook 'evil-insert-state)
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
    (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
    (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    ; buffer swapping, next/previous
    (define-key evil-normal-state-map (kbd "<RET>") 'me:switch-to-previous-buffer)
    ; esc key (from WikEmacs)
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    ; scroll keeping cursor in place
    (define-key evil-normal-state-map (kbd "C-j")
      (lambda () (interactive)  (evil-scroll-line-down 1) (evil-next-visual-line 0)))
    (define-key evil-normal-state-map (kbd "C-k")
      (lambda () (interactive) (evil-scroll-line-up 1) (evil-previous-visual-line 0)))
    ; Overload shifts so that they don't lose the selection
    (define-key evil-visual-state-map (kbd ">") 'me:evil-shift-right-visual)
    (define-key evil-visual-state-map (kbd "<") 'me:evil-shift-left-visual)
    (define-key evil-visual-state-map [tab] 'me:evil-shift-right-visual)
    (define-key evil-visual-state-map [S-tab] 'me:evil-shift-left-visual)
    (evil-mode 1)))


(use-package diminish
  :config
  (progn
    (diminish 'visual-line-mode)
    (diminish 'auto-revert-mode)
    (diminish 'undo-tree-mode)))

(use-package ws-butler
  :config
  (progn
    (ws-butler-global-mode t))
  :diminish ws-butler-mode
  :defer 3)

(use-package shell-pop
  :init
  (progn
    (setq shell-pop-internal-mode "ansi-term"
          shell-pop-term-shell "/bin/bash"
          shell-pop-window-size 40
          shell-pop-window-position "top"
          shell-pop-universal-key "<f8>"))
  :bind
  ("<f8>" . shell-pop))

(use-package which-key
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom))
  :defer 2
  :diminish which-key-mode)

(use-package magit
  :init
  :config
  (progn
    (add-hook 'with-editor-mode-hook (lambda () (setq fill-column 70)))
    ;get out of magit blame mode
    (define-key magit-blame-mode-map (kbd "<f7> b") 'magit-blame-quit)
    (use-package evil-magit))
  :bind
  (("<f7> g" . magit-status)
   ("<f7> b" . magit-blame)
   ("<f7> a" . magit-run-git-gui-blame)))

(use-package neotree
  :config
  (progn
    (when neo-persist-show
        (add-hook 'popwin:before-popup-hook
                  (lambda () (setq neo-persist-show nil)))
        (add-hook 'popwin:after-popup-hook
                  (lambda () (setq neo-persist-show t))))
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
    )
  :bind
  (("<f4> /" . neotree-toggle)
   ("<f7> /" . neotree-projectile-action)))

(use-package popwin
  :init
  (progn
    (setq display-buffer-function 'popwin:display-buffer))
  :config
  (progn
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
    (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
    (popwin-mode 1)))

(use-package linum-relative
  :init
  (progn
    (setq linum-relative-current-symbol ""))   ; show current line #
  :bind
  (("<f4> 3" . linum-relative-mode)))

(use-package ruler-mode
  :bind
  (("<f4> 6" . ruler-mode)))
