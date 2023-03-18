;;; packages-cedet.el --- Initialize the cedet mode

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Configure the cedet mode

;;; History:

;;; Code:
(use-package cedet
  :disabled
  :config
  (progn
    (require 'cedet)
    ;; Turn on EDE (Project handling mode)
    (global-ede-mode t)
    (semantic-mode 1)))

(provide 'packages-cedet)
