
(require 'package)

(setq package-archives '(
	("melpa" . "http://melpa.milkbox.net/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
	(package-refresh-contents))

(when (not (package-installed-p 'use-package ))
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; load utility packages
(use-package s)

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
(setq gdb-many-windows t)               ; use more complex gdb layout
(mouse-avoidance-mode 'animate)         ; move mouse pointer out of way
(column-number-mode t)                  ; display column/row of cursor in mode-line
(display-time-mode t)                   ; display time in mode-line
(setq frame-title-format '(             ; set title
		"☮ "
		(:eval (if (buffer-file-name)
					(s-replace (abbreviate-file-name default-directory) "" (abbreviate-file-name buffer-file-name))
					"%b"))
		" %* [" (:eval (abbreviate-file-name default-directory)) "]"
	  ))

;; tame scrolling
(setq scroll-margin 5                               ; leave 5 lines at top/bottom
		scroll-conservatively 100                     ; scroll # to bring point in view
		scroll-preserve-screen-position 'always       ; move cursor when scrolling
		mouse-wheel-scroll-amount '(3 ((shift) . 9))  ; 3 or 9 line when shift held
		mouse-wheel-follow-mouse 't                   ; scroll window under mouse
		mouse-wheel-progressive-speed nil)            ; don't speed up
(setq-default
		scroll-up-aggressively 0.01
		scroll-down-aggressively 0.01)

;; Operational preferences
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.cache/emacs")))
(setq custom-file "~/.cache/emacs/customize")       ; put customizations here

;; Text handling
(visual-line-mode t)                    ; edit visual lines
(setq default-tab-width 3)              ; ideal tab setting :)
;(add-hook 'text-mode-hook (             ; Hard-wrap text when in plaintext mode
;		lambda () (turn-on-auto-fill)))
(add-hook 'focus-out-hook (             ; save on focus lost
		lambda ()
			(interactive)
			(save-some-buffers t)))

(use-package whitespace
	:init
		(setq whitespace-line-column 120)
		(setq whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab))
		(setq  whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
		(global-whitespace-mode t)
	)

(use-package leuven-theme
	:requires whitespace
	:config
		(set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background "white" )
		(set-face-attribute 'whitespace-trailing nil :foreground "black" :background "red" )
	)

(defun switch-to-previous-buffer ()
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package semantic)

;; N.B. demand loaded because invoked via evil-leader
(use-package helm
	:init
		(setq helm-split-window-in-side-p t
			helm-M-x-fuzzy-match t
			helm-buffers-fuzzy-matching t
			helm-recentf-fuzzy-match t
			helm-locate-fuzzy-match t
			helm-apropos-fuzzy-match t
			helm-move-to-line-cycle-in-source t
			helm-ff-search-library-in-sexp t
			helm-buffer-max-length 40
			helm-scroll-amount 8)
	:config
	:bind ("M-x" . helm-M-x)
	:bind ("C-x b" . helm-for-files)
	:bind ("<f3>" . helm-for-files)
	:demand t)

;; N.B. demand loaded because invoked via evil-leader
(use-package projectile
	:init
		(setq projectile-completion-system 'helm)
		(setq projectile-enable-caching t)
		(setq projectile-switch-project-action 'projectile-find-file)
		; (setq projectile-switch-project-hook
		(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
	:config
		(projectile-global-mode 1)
	:bind ("<f7>" . projectile-compile-project)
	:demand t)

(use-package ag)
(use-package helm-ag)
(use-package grep)

(use-package helm-projectile
	:config
		(helm-projectile-on)
	:requires helm
	:requires projectile
	:bind ("<f5>" . helm-projectile-find-other-file))

(use-package helm-gtags
	:init
		(setq helm-gtags-auto-update t)
		(setq helm-gtags-use-input-at-cursor t)
		(setq helm-gtags-ignore-case t)
	:requires helm
	:config
		(add-hook 'dired-mode-hook 'helm-gtags-mode)
		(add-hook 'eshell-mode-hook 'helm-gtags-mode)
		(add-hook 'c-mode-hook 'helm-gtags-mode)
		(add-hook 'c++-mode-hook 'helm-gtags-mode)
		(add-hook 'asm-mode-hook 'helm-gtags-mode)
	)

(use-package markdown-mode
	:init
		;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
	)

(use-package cmake-mode
	:init
		(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
		(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
	)

(use-package smart-tabs-mode
	:config
		(smart-tabs-insinuate 'c 'c++)
	)

(use-package cc-mode
	:init
		(setq c-default-style "k&r" c-basic-offset=3)
		(setq show-paren-mode 0)
	:config
		(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
	:mode
		("\\.c\\'" . cc-mode)
		("\\.cpp\\'" . c++-mode)
		("\\.cxx\\'" . c++-mode)
		("\\.h\\'" . c++-mode)
		("\\.hpp\\'" . c++-mode)
	)

(use-package python-mode
	:config
		(smart-tabs-advice py-indent-line py-indent-offset)
		(smart-tabs-advice py-newline-and-indent py-indent-offset)
		(smart-tabs-advice py-indent-region py-indent-offset)
		(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
		(add-hook 'python-mode-hook
			(lambda ()
				(semantic-mode t)
				(setq indent-tabs-mode t)
				(setq tab-width 4)))
	)

(use-package git-gutter
	:config
		(global-git-gutter-mode t)
	)

(use-package deft
	:requires markdown-mode
	:init
		(setq deft-directory "~/Dropbox/Notes")
		(setq deft-recursive t)
		(setq deft-use-filter-string-for-filename t)
		(setq deft-file-naming-rules '((nospace . "_")
												 (noslash . "_")
												 (case-fn . downcase)))
		(setq deft-text-mode 'markdown-mode)
		; first extension in list seems to be used for new files.  Not sure
		; what deft-default-extesion does
		(setq deft-default-extension "md")
		(setq deft-extensions '("md" "txt" "text" "markdown" "mmd" "org"))

	:config
		(add-hook 'deft-mode-hook (lambda ()
											 (define-key deft-mode-map (kbd "<C-return>") 'deft-new-file)
											 (define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-clear)))
	)

(use-package company
	:config
		(add-hook 'after-init-hook 'global-company-mode)
	)

;broken due to changes in helm
;(use-package helm-company
;	:requires company
;	:requires helm)

(use-package neotree
	:bind ("<f4>" . neotree-toggle)
	:config
		(add-hook 'neotree-mode-hook
			(lambda ()
				(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
				(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
				(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
				(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
	)

; start deft in evil insert mode
(defun evil-deft ()
	(interactive)
	(deft)
	(evil-insert-state))

;; N.B. evil-mode must be enabled after global-evil-leader-mode
(use-package evil
	:init
		(setq evil-shift-width 3)
		(setq-default evil-symbol-word-search t)
		(setq evil-search-module 'evil-search)
	:config
		(use-package evil-args)
		(use-package evil-commentary :config (evil-commentary-mode))
		(use-package evil-leader
			:init
				(setq evil-leader/in-all-states 1)
			:config
				(global-evil-leader-mode t)
				(evil-leader/set-leader "<SPC>")
				(evil-leader/set-key
					";" 'evil-jump-forward
					"," 'evil-jump-backward
					"<SPC>" 'projectile-find-file
					"a" 'helm-projectile-find-other-file
					"bw" 'save-buffer
					"bq" 'kill-buffer-and-window
					"ci" 'projectile-invalidate-cache
					"ee" 'pp-eval-last-sexp
					"fa" 'helm-apropos
					"fb" 'projectile-switch-to-buffer
					"fd" 'projectile-find-dir
					"ff" 'projectile-find-file
					"fg" 'helm-projectile-ag
					"fi" 'helm-semantic-or-imenu
					"fj" 'helm-all-mark-ring
					"fk" 'helm-show-kill-ring
					"fm" 'helm-man-woman
					"fp" 'helm-list-elisp-packages-no-fetch
					"fx" 'helm-top
					"f/" 'helm-locate
					"gb" 'vc-annotate
					"ls" 'dired-jump
					"n" 'evil-deft
					"p" 'projectile-switch-project
					"tt" 'helm-gtags-dwim
					"tr" 'helm-gtags-find-rtag
					"td" 'helm-gtags-find-tag
					"ts" 'helm-gtags-find-symbol
					"tu" 'helm-gtags-update-tags
					"v"  'exchange-point-and-mark
					"wv" 'split-window-right
					"wh" 'split-window-below
					"x" 'helm-M-x
					)
			)  ; evil-leader

		(define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
		(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
		(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
		(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
		; buffer swapping, next/previous
		(define-key evil-normal-state-map (kbd "<RET>") 'switch-to-previous-buffer)
		; esc key (from WikEmacs)
		(define-key evil-normal-state-map [escape] 'keyboard-quit)
		(define-key evil-visual-state-map [escape] 'keyboard-quit)
		(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
		; scroll keeping cursor in place
		(define-key evil-normal-state-map (kbd "C-j")
			(lambda () (interactive)  (evil-scroll-line-down 1) (evil-next-visual-line 0)))
		(define-key evil-normal-state-map (kbd "C-k")
			(lambda () (interactive) (evil-scroll-line-up 1) (evil-previous-visual-line 0)))
		(evil-mode 1)
	) ; evil

(use-package powerline-evil
	:config
		(powerline-default-theme)
		(display-time-mode t)
	)

(use-package diminish
	:config
		(diminish 'company-mode)
		(diminish 'git-gutter-mode)
		(diminish 'undo-tree-mode)
		(diminish 'evil-commentary-mode)
		(diminish 'whitespace-mode)
		(diminish 'helm-gtags-mode)
	)
