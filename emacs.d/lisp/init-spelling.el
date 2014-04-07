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
;; * Now set the ispell-local-dictionary-alist to fix hunspell not
;;   working on windows.

(message "init-spelling -- Initialize the spelling mode...")

(setq ispell-program-name "hunspell")
(when (eq system-type 'windows-nt)
  (setq ispell-local-dictionary-alist
	'((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))))

;; Turn flyspell programming mode on
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook
	  (lambda () (flyspell-prog-mode)))

(provide 'init-spelling)
