;;; semx:
(mrb:package-install 'smex)
(add-hook 'after-init-hook
	  (lambda ()
	    (smex-initialize)
	    (global-set-key (kbd "M-x") 'smex)
	    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
	    (global-set-key (kbd "C-c M-x") 'execute-extended-command)))
