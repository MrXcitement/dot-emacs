;;; csharp-mode:
(mrb:package-install 'csharp-mode)
(message "package: csharp-mode installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: csharp-mode initializing...")
    (setq auto-mode-alist
	  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))) t)
