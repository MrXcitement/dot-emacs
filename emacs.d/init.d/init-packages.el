;;; init-packages.el --- Initialize the package manager and the installed packages

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.


;;; History:
;; 2014.11.12
;; * removed loading message

;; 2015.01.27
;; * require the use-package package to handle package management

;; 2015.03.13 (Friday the 13th!!!)
;; * require the auto-install package to handle packages not handled by package.el

;; 2015-06-03
;; * going forward all new package install/configuration scripts
;;   should be placed in the pacakages.d directory.
;; * moved visual-basic-mode.el from auto-install.d to packages.d directory
;; * removed auto-install.d directory


;;; Utility functions

;; Use mrb:package-install to only install a package that is not allready installed
(defun mrb:package-install (package-name)
  "Install a package from the package archives."
  (interactive "P")
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))

;; Check if the auto installed library exists
(defun mrb:auto-install-library-exists-p (library-name)
  (file-exists-p (format "%s/%s" auto-install-directory library-name)))

;; Install from url helper function
(defun mrb:auto-install-from-url (library-name library-url)
       (auto-install-from-url (format "%s/%s" library-url library-name)))


;;; Configure the package manager

;; Initialize the package-archives to be used.
(setq
 package-archives
 '(("gnu" .       "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" .     "http://melpa.org/packages/")))

;; Higlight the selected package
(add-hook 'package-menu-mode-hook
	  (lambda() (hl-line-mode 1)))

;; Initialize the package manager and installed packages.
(package-initialize)


;;; Bootstrap `use-package'
(mrb:package-install 'use-package)
(require 'use-package nil t)


;;; Bootstrap `auto-install'
(mrb:package-install 'auto-install)
(require 'auto-install nil t)

;; Do not prompt to save auto-installed libraries
(setq auto-install-save-confirm nil)

;; Set the auto-install download directory
(setq auto-install-directory
      (expand-file-name "auto-install/" user-emacs-directory))

;; If auto-install-directory is not in the load-path, add it
(unless (member auto-install-directory load-path)
  (add-to-list 'load-path auto-install-directory))


;;; Load package scripts defined in package.d
(mrb:load-directory
 (expand-file-name "init.d/packages.d" user-emacs-directory))

(provide 'init-packages)
;;; init-packages.el ends here.
