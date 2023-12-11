;;; init-lock-buffers.el --- Protect system buffers

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Lock the `*scratch*' and `*Messages*' buffers so they can not be killed.

;;; History:
;; 2023.03.22
;; * modify header to include standard sections.
;; 2014.11.12
;; * removed loading message

;;; Code:
(save-excursion
  (set-buffer "*scratch*")
  (emacs-lock-mode 'kill)
  (set-buffer "*Messages*")
  (emacs-lock-mode 'kill))

(provide 'init-lock-buffers)
;;; init-lock-buffers.el
