;;; lua-mode:
(mrb:package-install 'lua-mode)
(eval-after-load 'lua-mode-autoloads
  (progn
    (setq auto-mode-alist
	  (append '(("\\.\\(lua\\)$" .
		     lua-mode)) auto-mode-alist))))
