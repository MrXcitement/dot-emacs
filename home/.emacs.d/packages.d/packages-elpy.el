;;; packages-elpy.el --- Install and configure the `elpy' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Provides a python programming toolset.

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2015.05.15
;; * First release.

;;; Code:
(when (or (executable-find "python")
	  (executable-find "python3"))
  (use-package elpy
    :ensure t
    :defer t
    :init
    (advice-add 'python-mode :before 'elpy-enable)))


(provide 'packages-elpy)
