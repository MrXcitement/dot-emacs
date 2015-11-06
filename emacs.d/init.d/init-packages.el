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

;; 2015.06.03
;; * going forward all new package install/configuration scripts
;;   should be placed in the pacakages.d directory.
;; * moved visual-basic-mode.el from auto-install.d to packages.d directory
;; * removed auto-install.d directory

;; 2015.11.04 MRB
;; * Major edits.
;; * Added advice to package-install to refresh and retry
;;   package-install when package-install sends a file-error signal.
;; * Added mrb:package-desc and mrb:package-delete. mrb:package-desc
;;   is usefull to get the full description of a package from it's
;;   name and is used by mrb:package-delete to allow a package to be
;;   deleted outside the list-package interface.
;; * Added doc strings to function definitions.


;;; Utility functions

;;; Advice for 'package-install to change it to handle file-error signals.
(defun mrb:package-install-advice (orig-func &rest args)
"Advice for 'package-install to change how it handles file-error signals.
- If package-install does not signal an error:  just exit like normal.
- If package-install signals a file-error: refresh the package-archive and try package-install again.
- If package-install signals any other error: the signal is passed on.
- If the second package-install signals any error: the signal passed on.

This should fix errors where the package-archive has not been refreshed in a while and has become stale with invalid package version information.

Use the following commands to add/remove the advice:
(advice-add 'package-install :around #'mrb:package-install-advice)
(advice-remove 'package-install #'mrb:package-install-advice)"
  (condition-case err
      (apply orig-func args)
    (file-error
     (progn
       (message "Error: %S" err)
       (package-refresh-contents)
       (apply orig-func args)))))

;;; Add advice to 'package-install to handle a file-error when installing a package
(advice-add 'package-install :around #'mrb:package-install-advice)

;;; Get the package description from a package name
(defun mrb:package-desc (package)
  "Given a PACKAGE name, return the package's description."
  (car (cdr (assq 'neotree package-alist))))

;;; Delete a package
(defun mrb:package-delete (package)
  "Given a PACKAGE name, delete the package."
  (package-delete (mrb:package-desc package))
  (package-initialize))

;;; Use mrb:package-install to only install a package that is not allready installed
(defun mrb:package-install (package)
  "Given a PACKAGE name, install the package."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Check if the auto installed library exists
(defun mrb:auto-install-library-exists-p (library-name)
  "Given a LIBRARY-NAME check if it exists."
  (file-exists-p (format "%s/%s" auto-install-directory library-name)))

;; Install from url helper function
(defun mrb:auto-install-from-url (library-name library-url)
  "Given a LIBRARY-NAME and LIBRARY-URL, use auto-install to install the library."
  (auto-install-from-url (format "%s/%s" library-url library-name)))


;;; Configure the package manager

;; Initialize the package-archives to be used.
(setq
 package-archives
 '(("gnu" .       "http://elpa.gnu.org/packages/")
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


(byte-recompile-directory (expand-file-name "init.d/packages.d" user-emacs-directory) 0)

;;; Load package scripts defined in package.d
(mrb:load-directory
 (expand-file-name "init.d/packages.d" user-emacs-directory))

(provide 'init-packages)
;;; init-packages.el ends here.
