;;; packages-markdown --- Install and configure the `markdown' package.

;; Mike Barker <mike@thebarkers.com>

;;; Commentary:

;;; History:

;;; Code:
(use-package markdown-mode
  :ensure t
  :mode "\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$")

(provide 'packages-markdown)
