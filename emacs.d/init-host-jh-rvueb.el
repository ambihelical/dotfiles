;; initialization for host jh-rvueb
;; (message "jh-rvueb initialization")
(add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode t)))

;; make all of ~/dev/ have tabs be 4 characters wide
(dir-locals-set-class-variables 'me:work-local-variables
					  '((nil . ((dtrt-indent-force-offset . 4)))))
(dir-locals-set-directory-class "~/dev/" 'me:work-local-variables)
