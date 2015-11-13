;;; flyspell-popup.el --- Provision and configure the flyspell-popup
;; A mode to show flyspell correction choices in a popup

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: November 12th, 2015

;; History:
;; 2015.11.12
;; * First release.

;;; Install and use the flyspell-popup mode
(let ((package 'flyspell-popup))
  (package-install package)
  (use-package package
    :bind ("C-c $" . flyspell-popup-correct)))
