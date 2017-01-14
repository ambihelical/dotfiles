;;; Commentary:

;; Miscellaneous commands which can be demand loaded

;;; Code:

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
  "Select the nth other buffer"
  (interactive "P")
  (let ((buffer (nth arg (-filter 'me:useful-buffer (buffer-list)))))
    (if buffer
        (if prefix
            (switch-to-buffer-other-window buffer)
          (switch-to-buffer buffer)))))

;;;###autoload
(defun me:find-other-file (&optional prefix)
  "Find other file e.g. .h <-> .cpp"
  (interactive "P")
  (let ((case-fold-search nil))
    (if prefix
        (projectile-find-other-file-other-window)
      (projectile-find-other-file))))

;;;###autoload
(defun me:rotate-fill-column ()
  "Rotate the current fill-column between 70 and 120 incrementing by 10"
  (interactive)
  (setq fill-column (cond ((< fill-column 120) (+ fill-column 10))
                          ((>= fill-column 120) 70)))
  (message "fill-column is %s" fill-column)
  (whitespace-mode -1)
  (whitespace-mode))

(defconst me:powerline-separators #'( alternate arrow arrow-fade bar box brace butt chamfer contour curve rounded roundstub wave zigzag utf-8))
(defvar me:current-powerline-separator-index 12)               ; current powerline separator index

;;;###autoload
(defun me:next-powerline-separator ()
  "Rotate the powerline seperator style"
  (interactive)
  (if (not me:current-powerline-separator-index)
      (setq me:current-powerline-separator-index 0))
  (setq me:current-powerline-separator-index (% (+ me:current-powerline-separator-index 1) (length me:powerline-separators)))
  (setq powerline-default-separator (nth me:current-powerline-separator-index me:powerline-separators))
  (message "%s" powerline-default-separator)
  (spaceline-compile))

(provide 'me:extras)
