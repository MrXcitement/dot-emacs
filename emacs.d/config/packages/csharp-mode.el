;;; csharp-mode:
(mrb:package-install 'csharp-mode)
(add-hook 'after-init-hook
  (lambda()
    (setq auto-mode-alist
	  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))) t)
