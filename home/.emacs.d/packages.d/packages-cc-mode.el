;;; packages-cc-mode.el --- Configure cc-mode settings

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Configure the c major mode

;;; History:
;; 2014.10.23
;; * First release.

;;; Code:
(use-package cc-mode
  :init
  ;; hook c-mode to comment multi line regions like so:
  ;; /*
  ;;  * a multi-line
  ;;  * region comment
  ;;  */
  (add-hook 'c-mode-hook
	    (lambda() (setq comment-style 'extra-line))))

(provide 'packages-cc-mode)
