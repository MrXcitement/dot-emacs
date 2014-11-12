;;; elpy:
(mrb:package-install 'elpy)
(add-hook 'after-init-hook
  (lambda()
    (elpy-enable)) t)
