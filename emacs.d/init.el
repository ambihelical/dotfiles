
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

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(setq use-package-always-ensure t)

;(setq use-package-minimum-reported-time 0.03
;		use-package-verbose t)

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

; make some keys available for use
(global-unset-key (kbd "<f4>"))


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
	:diminish global-whitespace-mode)

(use-package leuven-theme
	:config
		(set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background "white" )
		(set-face-attribute 'whitespace-trailing nil :foreground "black" :background "red" )
	)

(defun switch-to-previous-buffer ()
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package semantic
  :config
    (semantic-mode t)
  :defer 5)

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
	:bind
		("M-x" . helm-M-x)
		("C-x b" . helm-for-files)
		("<f3>" . helm-for-files)
		("<S-f3>" . dired-jump)
		("<f4> a" . helm-apropos)
		("<f4> d" . helm-semantic-or-imenu)
		("<f4> j" . helm-all-mark-ring)
		("<f4> i" . helm-info-at-point)
		("<f4> k" . helm-show-kill-ring)
		("<f4> m" . helm-man-woman)
		("<f4> p" . helm-list-elisp-packages-no-fetch)
		("<f4> x" . helm-top)
		("<f4> l" . helm-locate)
	)

(use-package projectile
	:init
		(setq projectile-completion-system 'helm)
		(setq projectile-enable-caching t)
		(setq projectile-switch-project-action 'projectile-find-file)
		; (setq projectile-switch-project-hook
		(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
	:config
		(projectile-global-mode 1)
		(use-package helm-projectile
			:config
				(helm-projectile-on)
				;; show projectile info in helm-for-files when in a project
				;;(add-to-list 'helm-for-files-preferred-list helm-source-projectile-projects)
				(add-to-list 'helm-for-files-preferred-list helm-source-projectile-files-list)
				;;(add-to-list 'helm-for-files-preferred-list helm-source-projectile-directories-list)
			)
		(use-package ag)
		(use-package helm-ag)
		(use-package grep)

	:bind
		("<f4> g" . helm-projectile-ag)
		("<f5>" . helm-projectile-find-other-file)
		("<f7> <f7>" . projectile-switch-project)
		("<f7> c" . projectile-compile-project)
		("<f7> u" . projectile-invalidate-cache)
	)

(use-package helm-gtags
	:init
		(setq helm-gtags-auto-update t)
		(setq helm-gtags-use-input-at-cursor t)
		(setq helm-gtags-ignore-case t)
	:config
		(add-hook 'dired-mode-hook 'helm-gtags-mode)
		(add-hook 'eshell-mode-hook 'helm-gtags-mode)
		(add-hook 'c-mode-hook 'helm-gtags-mode)
		(add-hook 'c++-mode-hook 'helm-gtags-mode)
		(add-hook 'asm-mode-hook 'helm-gtags-mode)
	:bind
		("<f6>" . helm-gtags-dwim)
		("<S-f6>" . helm-gtags-update-tags )
	:diminish helm-gtags-mode)

(use-package markdown-mode
	:mode
		("\\.markdown\\'" . markdown-mode)
		("\\.md\\'" . markdown-mode)
	)

(use-package cmake-mode
	:mode
		("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode)
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
	:defer 5)

(use-package git-gutter
	:config
		(global-git-gutter-mode t)
	:defer 1
	:diminish git-gutter-mode)

(defun start-deft-in-evil-insert-mode ()
	(interactive)
	(deft)
	(evil-insert-state))

(use-package deft
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

	:bind
		("<f4> n" . start-deft-in-evil-insert-mode)

	:config
		(add-hook 'deft-mode-hook (lambda ()
											 (define-key deft-mode-map (kbd "<C-return>") 'deft-new-file)
											 (define-key deft-mode-map (kbd "<C-backspace>") 'deft-filter-clear)))
	)

(use-package company
	:config
		(add-hook 'after-init-hook 'global-company-mode)
	:diminish company-mode
	)

;broken due to changes in helm
;(use-package helm-company
;	:requires company
;	:requires helm)

;; N.B. evil-mode must be enabled after global-evil-leader-mode
(use-package evil
	:init
		(setq evil-shift-width 3)
		(setq-default evil-symbol-word-search t)
		(setq evil-search-module 'evil-search)
	:config
		(use-package evil-args)
		(use-package evil-commentary
		  :config (evil-commentary-mode)
		  :diminish evil-commentary-mode)
		(use-package evil-leader
			:init
				(setq evil-leader/in-all-states 1)
			:config
				(global-evil-leader-mode t)
				(evil-leader/set-leader "<SPC>")
				(evil-leader/set-key
					";" 'evil-jump-forward
					"," 'evil-jump-backward
					"bw" 'save-buffer
					"bq" 'kill-buffer-and-window
					"ee" 'pp-eval-last-sexp
					"gb" 'vc-annotate
					"v"  'exchange-point-and-mark
					"wv" 'split-window-right
					"wh" 'split-window-below
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
		(diminish 'undo-tree-mode)
	)
