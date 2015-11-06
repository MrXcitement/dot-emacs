;;; ido.el --- Initialize the Interactively DO mode

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message


;;; Configure the ido package
(use-package ido
  :config
  (progn
    (setq ido-enable-flex-matching t) ; enable fuzzy matching
    (setq ido-everywhere t)
    (ido-mode 1)))

;;; ido.el ends here.
