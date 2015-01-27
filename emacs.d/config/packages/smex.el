;;; semx:

;; Provide a smart M-x enhancement for Emacs using Ido.

(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))
