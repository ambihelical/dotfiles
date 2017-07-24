;;; -*- lexical-binding: t -*-
;;; Commentary:

;; Utility functions and commands

;;; Code:

;; save any dirty buffers
;;;###autoload
(defun me:save-dirty-buffers ()
  "Save any dirty buffers"
  (interactive)
  (save-some-buffers t))

;;;###autoload
(defun me:find-some-files ()
  "Find files in project or fallback to current directory"
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-find-file)
    (counsel-find-file)))

;; Return t if buffer can be selected.  This allows
;; certain non-file buffers to be selected with
;; me:select-nth-other-buffer.
(defun me:useful-buffer (buffer)
  (let ((bufname (buffer-name buffer))
        (specials `("*scratch*" "*Messages*")))
    (cond ((car (member bufname specials)) t)
          ((buffer-file-name buffer) t))))

;;;###autoload
(defun me:select-nth-other-buffer (arg &optional prefix)
  "Select the nth other buffer. Use prefix to put in other window"
  (interactive "P")
  (let ((buffer (nth arg (-filter 'me:useful-buffer (buffer-list)))))
    (if buffer
        (if prefix
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer)))))

;;;###autoload
(defun me:select-2nd-other-buffer (&optional prefix)
  "Select 2nd other buffer"
  (interactive "P")
  (me:select-nth-other-buffer 1 prefix))

;;;###autoload
(defun me:select-3rd-other-buffer (&optional prefix)
  "Select 2nd other buffer"
  (interactive "P")
  (me:select-nth-other-buffer 2 prefix))

;;;###autoload
(defun me:select-4th-other-buffer (&optional prefix)
  "Select 2nd other buffer"
  (interactive "P")
  (me:select-nth-other-buffer 3 prefix))

;;;###autoload
(defun me:select-5th-other-buffer (&optional prefix)
  "Select 2nd other buffer"
  (interactive "P")
  (me:select-nth-other-buffer 4 prefix))

;; set font attributes after theme loads
;;;###autoload
(defun me:set-extra-font-attributes ()
  (set-face-attribute 'default nil :background "gray99")
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
         (set-face-attribute 'hl-line nil :foreground 'unspecified :background "gainsboro")
         (set-face-attribute 'whitespace-line nil :foreground 'unspecified :background "lemon chiffon")
         (set-face-attribute 'whitespace-tab nil :foreground "gainsboro" :background bg )
         (set-face-attribute 'whitespace-trailing nil :foreground fg :background "red" )))

;; value of gc-cons-threshold to restore after minibuffer
(defconst me:normal-gc-cons-threshold gc-cons-threshold)

;; inhibit messages in echo area when minibuffer enabled
;; and set use large gc-cons-threshold
;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;;###autoload
(defun me:minibuffer-setup ()
  (setq inhibit-message t
        gc-cons-thresold most-positive-fixnum))

;; undo that done in me:minibuffer-setup
;;;###autoload
(defun me:minibuffer-exit ()
  (setq inhibit-message (> (minibuffer-depth) 1)
        gc-cons-threshold me:normal-gc-cons-threshold))

;;;###autoload
(defun me:paste-then-earlier ()
  "Paste and move to next kill ring entry (earlier in time)"
  (interactive)
  (evil-paste-after 1)
  (current-kill 1))

;;;###autoload
(defun me:paste-then-later ()
  "Paste and move to previous kill ring entry (later in time)"
  (interactive)
  (evil-paste-after 1)
  (current-kill -1))

(provide 'utilities)
