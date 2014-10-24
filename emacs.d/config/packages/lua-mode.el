;;; lua-mode:
(mrb:package-install 'lua-mode)
(message "package: lua-mode installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: lua-mode initializing...")
    (setq auto-mode-alist
	  (append '(("\\.\\(lua\\)$" .
		     lua-mode)) auto-mode-alist))) t)
