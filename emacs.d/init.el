
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

;; Fix operational annoyances
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.cache/emacs")))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)
(visual-line-mode 1)
(defun switch-to-previous-buffer ()
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer) 1)))
(setq gdb-many-windows t)
;; scrolling
(setq scroll-margin 5
      scroll-conservatively 100
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; Hard-wrap text when in plaintext mode
(add-hook 'text-mode-hook (lambda () (turn-on-auto-fill)))
(add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))
(setq default-tab-width 3)
(setq custom-file "~/.cache/emacs/customize")

;; pimping
(column-number-mode t) ; display column/row of cursor in mode-line
(desktop-save-mode t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode t)
(set-default-font "DejaVu Sans Mono 9")
(display-time-mode t)

(use-package s
	:ensure t)

(setq frame-title-format '(
		;;"" invocation-name ": "
		"☮ "
		(:eval (if (buffer-file-name)
					(s-replace default-directory "" buffer-file-name)
					"%b"))
		" [" (:eval (abbreviate-file-name default-directory)) "]"
	  ))

(use-package whitespace
	:init
		(setq whitespace-line-column 120)
		(setq whitespace-style '(face trailing tabs tab-mark lines-tail space-before-tab))
		(setq  whitespace-display-mappings '((tab-mark 9 [9657 9] [92 9])))
		(global-whitespace-mode t)
	:ensure t)

(use-package leuven-theme
	:config
		(use-package whitespace)
		(set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background "white" )
		(set-face-attribute 'whitespace-trailing nil :foreground "black" :background "red" )
	:ensure t)

(use-package evil
	:init
		(setq evil-shift-width 3)
		(setq-default evil-symbol-word-search t)
	:config
		(define-key evil-inner-text-objects-map "i" 'evil-inner-arg)
		(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
		(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
		(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
		(define-key evil-normal-state-map (kbd "<f6>") 'evil-jump-to-tag)   ;; deprecated--use leader-t instead
		; buffer swapping, next/previous
		(define-key evil-normal-state-map (kbd "<RET>") 'switch-to-previous-buffer)
		(define-key evil-normal-state-map (kbd "<backtab>") 'evil-prev-buffer)
		(define-key evil-normal-state-map (kbd "<tab>") 'evil-next-buffer)
		; esc key (from WikEmacs)
		(define-key evil-normal-state-map [escape] 'keyboard-quit)
		(define-key evil-visual-state-map [escape] 'keyboard-quit)
		(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
		(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
		; scroll keeping cursor in place
		(define-key evil-normal-state-map (kbd "C-k")
			(lambda () (interactive) (evil-scroll-line-down 1) (evil-next-visual-line 1)))
		(define-key evil-normal-state-map (kbd "C-j")
			(lambda () (interactive) (evil-scroll-line-up 1) (evil-previous-visual-line 1)))
		(evil-mode 1)
	:ensure t)

(use-package evil-leader
	:init
		(setq evil-leader/in-all-states 1)
	:config
		(global-evil-leader-mode 1)
		(evil-leader/set-leader "<SPC>")
		(evil-leader/set-key
			";" 'evil-jump-forward
			"," 'evil-jump-backward
			"<SPC>" 'projectile-find-file
			"a" 'helm-projectile-find-other-file
			"bs" 'save-buffer
			"bq" 'kill-buffer-and-window
			"ci" 'projectile-invalidate-cache
			"e" 'pp-eval-last-sexp
			"fa" 'helm-apropos
			"fb" 'projectile-switch-to-buffer
			"fd" 'projectile-find-dir
			"ff" 'projectile-find-file
			"fg" 'projectile-grep
			"fi" 'helm-semantic-or-imenu
			"fj" 'helm-all-mark-ring
			"fk" 'helm-show-kill-ring
			"fm" 'helm-man-woman
			"fp" 'helm-top
			"fP" 'helm-list-elisp-packages-no-fetch
			"fx" 'helm-M-x
			"f/" 'helm-locate
			"gw" 'global-whitespace-mode
			"ls" 'dired-jump
			"pp" 'projectile-switch-project
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
	:ensure t)

(use-package evil-args
	:config
		(use-package evil)
	:ensure t)

(use-package evil-commentary
	:config
		(use-package evil)
		(evil-commentary-mode)
	:ensure t)

(use-package semantic
	:ensure t)

(use-package helm
	:init
		(setq helm-split-window-in-side-p t
			helm-move-to-line-cycle-in-source t
			helm-ff-search-library-in-sexp t
			helm-scroll-amount 8
			helm-ff-file-name-history-use-recentf t)
	:config
		(global-set-key (kbd "M-x") 'helm-M-x)
		(global-set-key (kbd "C-x b") 'helm-mini)
		(global-set-key (kbd "<f3>") 'helm-mini)
		;(helm-mode t)
	:ensure t)

(use-package projectile
	:init
		(setq projectile-completion-system 'helm)
		(setq projectile-enable-caching t)
		(setq projectile-switch-project-action 'projectile-find-file)
		; (setq projectile-switch-project-hook
		(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
	:config
		(projectile-global-mode 1)
		(global-set-key (kbd "<f7>") 'projectile-compile-project)

	:ensure t)

(use-package helm-projectile
	:config
		(helm-projectile-on)
	:bind ("<f5>" . helm-projectile-find-other-file)
	:ensure t)

(use-package helm-gtags
	:init
		(setq helm-gtags-auto-update t)
		(setq helm-gtags-use-input-at-cursor t)

	:config
		(add-hook 'dired-mode-hook 'helm-gtags-mode)
		(add-hook 'eshell-mode-hook 'helm-gtags-mode)
		(add-hook 'c-mode-hook 'helm-gtags-mode)
		(add-hook 'c++-mode-hook 'helm-gtags-mode)
		(add-hook 'python-mode-hook 'helm-gtags-mode)
		(add-hook 'asm-mode-hook 'helm-gtags-mode)
	:ensure t)


(use-package smart-tabs-mode
	:config
		(smart-tabs-insinuate 'c 'c++)
	:ensure t)

(use-package cc-mode
	:init
		(setq c-default-style "k&r" c-basic-offset=3)
		(setq show-paren-mode 0)
	:config
		(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
		(use-package smart-tabs-mode)
	:mode
		("\\.c\\'" . cc-mode)
		("\\.cpp\\'" . c++-mode)
		("\\.cxx\\'" . c++-mode)
		("\\.h\\'" . c++-mode)
		("\\.hpp\\'" . c++-mode)
	:ensure t)

(use-package python
	:config
		(add-hook 'python-mode-hook
			(lambda ()
				(setq indent-tabs-mode t)
				(setq tab-width 4)))
	:ensure t)

(use-package git-gutter
	:config
		(global-git-gutter-mode)
	:ensure t)

(use-package company
	:config
		(add-hook 'after-init-hook 'global-company-mode)
	:ensure t)


(use-package neotree
	:bind ("<f4>" . neotree-toggle)
	:config
		(add-hook 'neotree-mode-hook
			(lambda ()
				(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
				(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
				(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
				(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
	:ensure t)


(use-package powerline-evil
	:config
		(powerline-default-theme)
		(display-time-mode t)
	:ensure t)


(use-package diminish
	:config
		(diminish 'company-mode)
		(diminish 'git-gutter-mode)
		(diminish 'undo-tree-mode)
		(diminish 'evil-commentary-mode)
		(diminish 'whitespace-mode)
	:ensure t)
