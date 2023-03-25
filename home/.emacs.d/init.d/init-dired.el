;;; init-dired.el --- Initialize the `dired' mode

;; Mike Barker <mike@thebarkers.com>
;; March 25th, 2023

;;; Commentary:
;; Highlight the current line.

;;; History:
;; 2023-03-25
;; * Created


;;; Code:

;; Highlight the current line when in dired mode.
(add-hook 'dired-mode-hook
	  (lambda() (hl-line-mode 1)))


(provide 'init-dired)
;;; End of init-dired.el
