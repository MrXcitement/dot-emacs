;; init-spelling.el -- initialize the spelling sub system.

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message


(setq ispell-program-name "hunspell")

(when (eq system-type 'darwin)
  (setenv "DICTIONARY" "en_US"))

(when (eq system-type 'windows-nt)
  (setq ispell-local-dictionary-alist
	'((nil "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))))

;; Turn flyspell programming mode on
;; (add-hook 'emacs-lisp-mode-hook
;; 	  (lambda () (flyspell-prog-mode)))
;; (add-hook 'python-mode-hook
;; 	  (lambda () (flyspell-prog-mode)))

(provide 'init-spelling)
;; init-spelling.el ends here.
