;;; environment.el --- Initialize the system environment

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message

;;; setup darwin (mac os x) environment setup here...
(when (eq system-type 'darwin)

  ;; Setup the path.
  (let ((mypaths '("~/bin"
		   "/Users/Shared/bin"
		   "/usr/local/share/python"
		   "/usr/local/git/bin"
		   "/usr/local/bin"
		   "/usr/local/sbin"
		   "/usr/bin"
		   "/usr/sbin"
		   "/bin"
		   "/sbin")))
    (setenv "PATH" (mapconcat 'identity mypaths ":"))
    (setq exec-path mypaths))

  ;; Force the current directory to be the users home dir
  (setq default-directory "~/")

  ;; Use the provided elisp version of ls
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;;; setup linux environment here...
(when (eq system-type 'linux))

;;; setup window environment here...
(when (eq system-type 'windows-nt))

;;; environment.el ends here.
