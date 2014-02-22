;; init-spelling.el -- initialize the spelling sub system.

;; Mike Barker <mike@thebarkers.com>
;; August 23, 2013

;; Copyright (c) 2013 Mike Barker 

;; Change log:
;; 2013.08.23
;; * First release.

(setq ispell-program-name "hunspell")

;; Turn flyspell mode on
(add-hook 'emacs-lisp-mode
	  (lambda()
	    (flyspell-prog-mode)))

(provide 'init-spelling)


