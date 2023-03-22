;;; init-packages.el --- Initialize the package manager and the installed packages

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Initialize and configure `package.el' package.
;; Define some utility functions that wrap `package.el' functions.
;; Initialize and configure the `use-package' package.
;; TODO: Find a replacement for the `auto-install.el' package

;;; History:
;; 2023-03-22 MRB
;; * modify the header to include standard sections.
;; * when emacs version is < 27 run `package-initialize'
;; * when emacs version is < 28 install `use-package'
;; * changed function namespace qualifier from `mrb:' to `my-'
;; 2021-03-15 MRB
;; Remove copyright notice
;; Comment out all functions that use auto-install, will look into a
;; replacement. Possibly straight.el?
;; 2015.11.04 MRB
;; * Major edits.
;; * Added advice to package-install to refresh and retry
;;   package-install when package-install sends a file-error signal.
;; * Added mrb:package-desc and mrb:package-delete. mrb:package-desc
;;   is usefull to get the full description of a package from it's
;;   name and is used by mrb:package-delete to allow a package to be
;;   deleted outside the list-package interface.
;; * Added doc strings to function definitions.
;; 2015.06.03
;; * going forward all new package install/configuration scripts
;;   should be placed in the pacakages.d directory.
;; * moved visual-basic-mode.el from auto-install.d to packages.d directory
;; * removed auto-install.d directory
;; 2015.03.13 (Friday the 13th!!!)
;; * require the auto-install package to handle packages not handled by package.el
;; 2015.01.27
;; * require the use-package package to handle package management
;; 2014.11.12
;; * removed loading message

;;; Code:

;; Advice for 'package-install to change it to handle file-error signals.
(defun my-package-install-advice (orig-func &rest args)
"Advice for 'package-install to change how it handles file-error signals.

- If package-install does not signal an error:  just exit like normal.
- If package-install signals a file-error: refresh the package-archive and try package-install again.
- If package-install signals any other error: the signal is passed on.
- If the second package-install signals any error: the signal passed on.

This should fix errors where the package-archive has not been refreshed in a while and has become stale with invalid package version information.

Use the following commands to add/remove the advice:
(advice-add 'package-install :around #'my-package-install-advice)
(advice-remove 'package-install #'my-package-install-advice)"
  (condition-case err
      (apply orig-func args)
    (file-error
     (progn
       (message "Error: %S" err)
       (package-refresh-contents)
       (apply orig-func args)))))

;; Add advice to 'package-install to handle a file-error when installing a package
(advice-add 'package-install :around #'my-package-install-advice)

;; Get a package's description from a package name
(defun my-package-desc (package)
  "Given a PACKAGE name, return the package's description."
  (car (cdr (assq 'neotree package-alist))))

;; Delete a package
(defun my-package-delete (package)
  "Given a PACKAGE name, delete the package."
  (package-delete (my-package-desc package))
  (package-initialize))

;; Install a package, but only if it is not allready installed
(defun my-package-install (package)
  "Given a PACKAGE name, install the package."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Initialize the package-archives to be used.
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; Higlight the selected package
(add-hook 'package-menu-mode-hook (lambda() (hl-line-mode 1)))

;; Initialize the package manager and installed packages.
(when (< emacs-major-version 27)
  (package-initialize))

;; Bootstrap `use-package'

;; Install and configure the `use-package' package.
(when (< emacs-major-version 28)
  (my-package-install 'use-package)
  (require 'use-package nil t))
(setq use-package-expand-minimally t)

;; Bootstrap `auto-install'

;; Install and configure the `auto-install' package.
;; Check if the auto installed library exists
;; (defun my-auto-install-library-exists-p (library-name)
;;   "Given a LIBRARY-NAME check if it exists."
;;   (file-exists-p (format "%s/%s" auto-install-directory library-name)))

;; Install from url helper function
;; (defun my-auto-install-from-url (library-name library-url)
;;   "Given a LIBRARY-NAME and LIBRARY-URL, use auto-install to install the library."
;;   (auto-install-from-url (format "%s/%s" library-url library-name)))

;; (my-package-install 'auto-install)
;; (require 'auto-install nil t)

;; Do not prompt to save auto-installed libraries
;; (setq auto-install-save-confirm nil)

;; Set the auto-install download directory
;; (setq auto-install-directory
;;       (expand-file-name "auto-install/" user-emacs-directory))

;; If auto-install-directory is not in the load-path, add it
;; (unless (member auto-install-directory load-path)
;;   (add-to-list 'load-path auto-install-directory))

(provide 'init-packages)
;;; init-packages.el ends here.
