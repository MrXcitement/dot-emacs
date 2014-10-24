;;; elpy:
(mrb:package-install 'elpy)
(message "package: elpy installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: elpy initializing...")
    (elpy-enable)) t)
