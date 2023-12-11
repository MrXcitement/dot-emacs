;; init-spelling.el -- initialize the spelling sub system.

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; When the spelling program exists, initialize the spelling system.

;;; History:
;; 2023.03.22
;; * modify header to include standard sections.
;; * check if spelling program exists before turning on spelling
;; 2014.11.12
;; * removed loading message

;;; Code:

(when (executable-find "hunspell")
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
  )

(provide 'init-spelling)
;; init-spelling.el ends here.
