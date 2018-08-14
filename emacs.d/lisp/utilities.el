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

;; Return non-nil if buffer can be selected.  This allows
;; certain non-file buffers to be selected with
;; me:select-nth-other-buffer.

(defconst me:special-names #'("*scratch*" "*Messages*" "*ielm*" "*Help*" "*info*"))
(defconst me:special-modes #'( dired-mode woman-mode ))

(defun me:useful-buffer (buffer)
  (let ((bufname (buffer-name buffer)))
    (cond ((member bufname me:special-names))             ;; one of these names
          ((buffer-file-name buffer))                     ;; has a file
          ((string-match ".~[-_.A-Za-z]+~$" bufname))     ;; magit branch buffer
          ((member (with-current-buffer buffer major-mode) me:special-modes)))))  ;; one of these modes

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
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
         (set-face-attribute 'whitespace-line nil :foreground 'unspecified :background "grey32")
         (set-face-attribute 'whitespace-tab nil :foreground "grey32" :background bg )
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

(provide 'utilities)
