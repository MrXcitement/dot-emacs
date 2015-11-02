;;; auto-complete:

;; This package provides auto complete in a drop down window similar
;; to intelisense in visual studio.

;; Use the auto-complete package
(use-package auto-complete
  :disabled
  :ensure t
  :init
  (progn
    ;; fixup problems with flyspell mode
    (defun mrb:auto-complete-flyspell-mode-hook()
      (ac-flyspell-workaround))

    ;; set the correct sources for ielm mode
    (defun mrb:auto-complete-ielm-mode-hook()
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

    (add-hook 'flyspell-mode 'mrb:auto-complete-flyspell-hook)
    (add-hook 'ielm-mode-hook 'mrb:auto-complete-ielm-mode-hook)
    (add-hook 'elpy-mode-hook 'mrb:auto-complete-elpy-mode-hook))

  :config
  (progn
    ;; set default configuration
    (require 'auto-complete-config)
    (ac-config-default)
    (global-auto-complete-mode t)
    (setq ac-auto-start 2)
    (setq ac-ignore-case nil))

  )
