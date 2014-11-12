;;; lua-mode:
(mrb:package-install 'lua-mode)
(add-hook 'after-init-hook
  (lambda()
    (setq auto-mode-alist
	  (append '(("\\.\\(lua\\)$" .
		     lua-mode)) auto-mode-alist))) t)
