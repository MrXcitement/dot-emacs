;; init-spelling.el -- initialize the spelling sub system.

;; Mike Barker <mike@thebarkers.com>
;; August 23, 2013

;; Copyright (c) 2013 Mike Barker

;; Change log:
;; 2013.08.23
;; * First release.

;; 2014-02-27 MRB
;; * Added initialize message
;; * Hook python mode to use flyspell programming mode

(message "init-spelling -- Initialize the spelling mode...")

(setq ispell-program-name "hunspell")

;; Turn flyspell programming mode on
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook
	  (lambda () (flyspell-prog-mode)))

(provide 'init-spelling)
