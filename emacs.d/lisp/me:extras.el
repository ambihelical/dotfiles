;;; Commentary:

;; Miscellaneous commands which can be demand loaded

;;; Code:

;;;###autoload
(defun me:select-nth-other-buffer (arg)
  "Select the nth other buffer"
  (interactive "p")
  (let ((buffer (nth arg (-filter 'buffer-file-name (buffer-list)))))
    (if buffer
        (switch-to-buffer buffer))))

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
