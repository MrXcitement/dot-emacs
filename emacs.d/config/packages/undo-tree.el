;;; undo-tree:

;; Provides a visulization to the undo buffer

(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)))
