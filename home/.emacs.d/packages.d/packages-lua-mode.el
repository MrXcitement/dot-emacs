;;; packages-lua-mode --- Install and configure the `lua-mode' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Provide support for editing lua code

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2015.05.15
;; * First release.

;;; Code:
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(provide 'packages-lua-mode)
