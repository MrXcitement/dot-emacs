;;; packages-flyspell-popup.el --- Install and configure the `flyspell-popup' package.

;; Mike Barker <mike@thebarkers.com>
;; November 12th, 2015

;;; Commentary:
;; A mode to show flyspell correction choices in a popup

;;; History:
;; 2023.03.16
;; * Renamed and reformated to be a valid package
;; * use-package installs the package now
;; 2015.11.12
;; * First release.

;;; Code:
(use-package flyspell-popup
  :ensure t
  :bind ("C-c $" . flyspell-popup-correct))

(provide 'packages-flyspell-popup)
