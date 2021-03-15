;;; cedet.el --- Initialize the cedet mode

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.


;;; Configure the cedet mode
(use-package cedet
  :config
  (progn
    (require 'cedet)
    ;; Turn on EDE (Project handling mode)
    (global-ede-mode t)
    (semantic-mode 1)))
;;; cedet.el ends here.
