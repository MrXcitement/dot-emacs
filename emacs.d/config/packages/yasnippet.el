;;; yasnippet:
(mrb:package-install 'yasnippet)
(message "package: yasnippet installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: yasnippet initializing...")
    (yas-global-mode 1)) t)
