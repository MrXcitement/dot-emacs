;;; elpy:
(mrb:package-install 'elpy)
(eval-after-load 'elpy-autoloads
  (progn
    (elpy-enable)))
