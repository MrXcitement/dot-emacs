;;; packages-ac-html.el --- Install and configure the `ac-html' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; A mode to edit html files.

;;; History:
;; 2023.03.22
;; * rename personal functions from `my/funcname' to `my-funcname'
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; * rename personal functions from mrb:funcname to my/funcname
;; 2015.05.15
;; * First release.

;;; Code:
(use-package ac-html
  :disabled
  :functions my-ac-html-hook
  :init
  (add-hook 'html-mode-hook 'my-ac-html-hook)
  :config
  (progn
    ;; my ac-html auto completion
    (defun my-ac-html-hook()
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

(provide 'packages-ac-html)
