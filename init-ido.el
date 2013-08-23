;; init-ido.el -- Initialize the Interactively DO mode 

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;; 2013-08-23 MRB
;; * No longer use custom ido-execute-command to use ido matching with
;;   M-x. Now using the package Smex to provide this capability.

;;; Interactively do things
(setq ido-enable-flex-matching t) ; enable fuzzy matching
(setq ido-everywhere t)
(ido-mode 1)

(provide 'init-ido)


