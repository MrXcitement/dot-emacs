;;; ntcmd:
(mrb:package-install 'ntcmd)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(bat\\|cmd\\)$" .
			     ntcmd-mode)) auto-mode-alist))))
