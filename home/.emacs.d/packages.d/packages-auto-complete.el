;;; packages-auto-complete.el --- Install and configure auto-complete

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; This package provides auto complete in a drop down window similar
;; to intelisense in visual studio.

;;; History:
;; * rename personal functions from `my/funcname' to `my-funcname'
;; 2023.03.17
;; * rename and refactor file to be a valid package
;; * rename personal functions

;;; Code:
(use-package auto-complete
  :disabled
  :ensure t
  :init
  (progn
    ;; fixup problems with flyspell mode
    (defun my-auto-complete-flyspell-mode-hook()
      (ac-flyspell-workaround))

    ;; set the correct sources for ielm mode
    (defun my-auto-complete-ielm-mode-hook()
      (setq ac-sources '(ac-source-functions
			 ac-source-variables
			 ac-source-features
			 ac-source-symbols
			 ac-source-words-in-same-mode-buffers))
      (add-to-list 'ac-modes 'inferior-emacs-lisp-mode))

    ;; turn off auto-complete menu when in elpy mode
    (defun mrb-auto-complete-elpy-mode-hook()
      (message "elpy-mode uses company-mode, turning off auto-complete menu...")
      (make-local-variable 'ac-auto-start)
      (setq ac-auto-start nil))

    (add-hook 'flyspell-mode 'my-auto-complete-flyspell-hook)
    (add-hook 'ielm-mode-hook 'my-auto-complete-ielm-mode-hook)
    (add-hook 'elpy-mode-hook 'my-auto-complete-elpy-mode-hook))

  :config
  (progn
    ;; set default configuration
    (require 'auto-complete-config)
    (ac-config-default)
    (global-auto-complete-mode t)
    (setq ac-auto-start 2)
    (setq ac-ignore-case nil)))

(provide 'packages-auto-complete)
