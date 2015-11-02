;;; omnisharp --- install and configure the omnisharp package

;; Provide support for editing C Sharp code.
(use-package omnisharp
  :disabled
  :if (and (>= emacs-major-version 24)(>= emacs-minor-version 4))
  :ensure t
  :config
  (progn
    (setq omnisharp-server-executable-path "~/git/OmniSharpServer/OmniSharb/bin/Debug")
    (add-hook 'csharp-mode-hook 'omnisharp-mode)))
