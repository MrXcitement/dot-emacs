;;; packages.el --- Initialize the package manager and the installed packages

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; Refresh the package database, but only if the package name is not found.
(defun mrb:package-refresh-contents (package)
  "Refresh the package database if the package name is not found."
  (let ((pkg-desc (assq package package-archive-contents)))
    (unless pkg-desc
      (message "Refreshing the package database")
      (package-refresh-contents))))

;;; Install a single package.
;;; Only install a package that is not allready installed
;;; 2013-05-02 MRB - Ignore errors when installing packages. This
;;; allows a system that does not have access to the various package
;;; archives to continue to work and not stop the initialization of
;;; emacs.
(defun mrb:package-install (package)
  "Install a single package."
  (unless (package-installed-p package)
    (message "Installing package: %s" package)
    (ignore-errors
      (mrb:package-refresh-contents package)
      (package-install package))))

;;; Install a list of packages.
(defun mrb:packages-install (package-list)
  "Install a list of packages."
  (loop for p in package-list do
	(mrb:package-install p)))

;;; Initialize the Package Manager
(message "packages -- Initializing emacs package manager...")

;;; Initialize the package-archives to be used.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Higlight the selected package
(add-hook 'package-menu-mode-hook
	  (lambda() (hl-line-mode 1)))

;;; Initialize the package manager and installed packages.
(package-initialize)

;;; pacakages.el ends here.
