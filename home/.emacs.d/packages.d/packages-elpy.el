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
(when (executable-find "python3")
  (setq python-shell-interpreter "python3"
	python-shell-interpreter-args "-i")
  (use-package elpy
    :ensure t
    :init
    (elpy-enable)))

(provide 'packages-elpy)
