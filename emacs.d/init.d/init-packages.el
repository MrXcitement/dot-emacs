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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package nil t)

;; Load package scripts defined in package.d
(mrb:load-directory
 (expand-file-name "init.d/packages.d" user-emacs-directory))

(provide 'init-packages)
;;; init-packages.el ends here.
