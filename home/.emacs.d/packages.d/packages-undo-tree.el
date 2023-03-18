;;; packages-undo-tree --- Initialize and configure the `undo-tree' package.

;; Mike Barker <mike@thebarkers.com>

;;; Commentary:
;; Provides a visulization to the undo buffer

;;; History:

;;; Code:
(use-package undo-tree
  :disabled
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)))

(provide 'packages-undo-tree)
