;;; ac-html.el --- Require and configure ac-html mode
;; A mode to edit html files.

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: May 15, 2015

;; History:
;; 2015.05.15
;; * First release.

(use-package ac-html
  :disabled
  :ensure t
  :init
  (add-hook 'html-mode-hook 'mrb:ac-html-hook)
  :config
  (progn
    ;; my ac-html auto completion
    (defun mrb:ac-html-hook()
      ;; Require ac-html since we are setup html auto completion
      (require 'ac-html)
      ;; Require default data provider if you want to use
      (require 'ac-html-default-data-provider)
      ;; Enable data providers,
      ;; currently only default data provider available
      (ac-html-enable-data-provider 'ac-html-default-data-provider)
      ;; Let ac-haml do some setup
      (ac-html-setup)
      ;; Set your ac-source
      (setq ac-sources '(ac-source-html-tag
			 ac-source-html-attr
			 ac-source-html-attrv))
      ;; Enable auto complete mode
      (auto-complete-mode))))
