;;; init-environment.el --- Initialize the system environment

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Configure environment settings

;;; History:
;; 2023.03.22
;; * modify header to include default sections.
;; * remove setting `exec-path' on `darwin' systems.
;; 2014.11.12
;; * removed loading message

;;; Code:

;; Darwin (mac os x) environment setup here...
(when (eq system-type 'darwin)
  ;; Force the current directory to be the users home dir
  (setq default-directory "~/")

  ;; Use the provided elisp version of ls
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Linux environment here...
(when (eq system-type 'linux))

;; Windows environment here...
(when (eq system-type 'windows-nt))

(provide 'init-environment)
;;; init-environment.el ends here.
