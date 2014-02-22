;; init-protbufs.el -- Protect system buffers

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Protect buffers
(save-excursion
  (set-buffer "*scratch*")
  (emacs-lock-mode 'kill)
  (set-buffer "*Messages*")
  (emacs-lock-mode 'kill))

(provide 'init-protbufs)
