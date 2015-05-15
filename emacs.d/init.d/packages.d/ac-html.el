;;; ac-html.el --- Require and configure ac-html mode
;; A mode to edit html files.

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: May 15, 2015

;; History:
;; 2015.05.15
;; * First release.

;;; ac-html auto completion
(use-package ac-html
  :ensure t
  :config
  (progn
    (add-hook 'html-mode-hook 'ac-html-enable)))
