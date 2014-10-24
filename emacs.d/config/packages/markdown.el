;;; markdown:
(mrb:package-install 'markdown-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$" .
			     markdown-mode)) auto-mode-alist))))
