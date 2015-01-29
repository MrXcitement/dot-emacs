;;; company-mode.el

;; Provide a text complition framework.

(use-package company
  :ensure t
  :bind
  ("<C-tab>" . company-complete-common)
  :config
  (global-company-mode))
