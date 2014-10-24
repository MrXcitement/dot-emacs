;;; markdown:
(mrb:package-install 'markdown-mode)
(message "package: markdown-mode installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: markdown-mode initializing...")
    (setq auto-mode-alist
	  (append '(("\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$" .
		     markdown-mode)) auto-mode-alist))) t)
