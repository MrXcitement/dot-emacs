;;; ntcmd:
(mrb:package-install 'ntcmd)
(message "package: ntcmd installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: ntcmd initializing...")
    (setq auto-mode-alist
	  (append '(("\\.\\(bat\\|cmd\\)$" .
		     ntcmd-mode)) auto-mode-alist))) t)
