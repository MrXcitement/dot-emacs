;;; packages-csharp-mode.el --- Install and configure the `csharp-mode' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Provide support for editing C Sharp code.

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2015.05.15
;; * First release.

;;; Code:
(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'")

(provide 'packages-csharp-mode)
