;;; undo-tree:
(mrb:package-install 'undo-tree)
(add-hook 'after-init-hook
  (lambda()
    (global-undo-tree-mode)) t)
