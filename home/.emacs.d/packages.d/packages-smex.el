;;; packages-smex.el --- Install and configure the `smex' package.

;; Mike Barker <mike@thebarkers.com>

;;; Commentary:
;; Provide a smart M-x enhancement for Emacs using Ido.

;;; History:

;;; Code:
(use-package smex
  :disabled
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

(provide 'packages-smex)
