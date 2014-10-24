;;; csharp-mode:
(mrb:package-install 'csharp-mode)
(eval-after-load 'csharp-mode-autoloads
  (progn
    (setq auto-mode-alist
	  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))))
