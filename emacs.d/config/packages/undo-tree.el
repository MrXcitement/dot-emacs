;;; undo-tree:
(mrb:package-install 'undo-tree)
(message "package: undu-tree installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: undu-tree initializing...")
    (global-undo-tree-mode)) t)
