;;; -*- lexical-binding: t -*-
;;; Commentary:

;; Infrequently used commands

;;; Code:


;;;###autoload
(defun me:find-other-file (&optional prefix)
  "Find other file e.g. .h <-> .cpp. Use prefix to put in other window."
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

;;;###autoload
(defun me:ps-two-per-page (&optional prefix)
  "Print buffer 2-up.  Use prefix to save to file"
  (interactive "P")
  (require 'ps-print)
  (let ((ps-n-up-printing    2)
        (ps-n-up-border-p    nil)
        (ps-spool-duplex     t)
        (ps-lpr-switches     '( "-o Duplex=DuplexTumble" ))   ;; short-side binding
        (ps-paper-type      'letter)
        (ps-font-size        (quote (8 . 11)))
        (ps-top-margin       -25)
        (ps-bottom-margin    -35)
        (ps-left-margin      22)
        (ps-right-margin     22)
        (ps-n-up-margin      1)
        (ps-inter-column     1))
    (if prefix (ps-print-buffer (read-file-name "file name:"))
      (ps-print-buffer))))

;;;###autoload
(defun me:ps-one-per-page (&optional prefix)
  "Print buffer 1-up.  Use prefix to save to file"
  (interactive "P")
  (require 'ps-print)
  (let ((ps-n-up-printing    1)
        (ps-n-up-border-p    nil)
        (ps-spool-duplex     t)
        (ps-paper-type      'letter)
        (ps-font-size        (quote (7 . 8.5)))
        (ps-top-margin       20)
        (ps-bottom-margin    20)
        (ps-left-margin      20)
        (ps-right-margin     20)
        (ps-n-up-margin      1)
        (ps-inter-column     1))
    (if prefix (ps-print-buffer (read-file-name "file name:"))
      (ps-print-buffer))))

;;;###autoload
(defun me:paste-then-earlier ()
  "Paste before and move to next kill ring entry (earlier in time)"
  (interactive)
  (evil-paste-before 1)
  (current-kill 1)
  (setq this-command #'evil-paste-before))   ; enable evil-repeat and evil-paste-pop

;;;###autoload
(defun me:paste-then-later ()
  "Paste after and move to previous kill ring entry (later in time)"
  (interactive)
  (evil-paste-after 1)
  (current-kill -1)
  (setq this-command #'evil-paste-after))   ; enable evil-repeat and evil-paste-pop

;;;###autoload
(defun x-urgency-hint (frame &optional reset )
  "Set urgency hint on given frame. RESET will reset it"
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if reset
                (logand flags (lognot #x100))
              (logior flags #x100)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))


(provide 'extras)
