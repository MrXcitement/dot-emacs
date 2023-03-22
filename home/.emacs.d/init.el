;;; init.el --- My Emacs initialization file

;; Mike Barker <mike@thebarkers.com>
;; October 10, 2007

;;; Commentary:
;; Initialize the emacs editor by requiring package files in the
;; `init.d' and `pacakges.d' folders. The actual configuration of the
;; emacs editor is handled by the files in these folders. The `init.d'
;; folder containes package files that initialize the core of the
;; emacs editor. The `package.d' folder contains packages to install
;; and configure third party packages using the `use-package' macro.

;;; History
;; 2023-03-22
;; * rename personal functions from `my/funcname' to `my-funcname'
;; 2023-03-18 MRB
;; - Now `require' files in the `init.d' and `packages.d' folders.
;; - Renamed files in the `packages.d' folder to be precided by packages-
;; - Added `provides' directive to the end of the files in `packages.d'
;; - Standardize the header and comments in the elisp files.
;; - Added ability to switch theme based on macOS dark mode status
;; - Added `vs-light' and `vs-dark' themes, replacing existing themes
;; - Moved spelling package into `init.d' folder
;; - Added `exec-path-from-shell' package.
;; 2021-03-15 MRB
;; Reorganize init files
;; - Moved packages from ~/.emacs.d/init.d to ~/.emacs.d/packages.d
;; - Now require any .el file in the init.d directory
;; - Load any .el file in the packages.d directory
;; - Misc changes to get the initialization to complete without errors
;; - Removed copyright in header

;;; Code:

;; Emacs 24 or greater only
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))


;; Require all `.el' files in a directory
(defun my-require-directory (directory)
  "Require all `.el' files in DIRECTORY."
  (when (file-exists-p directory)
    (add-to-list 'load-path directory)
    (dolist (file (directory-files directory nil "\\.el$"))
      (message "require file %s..." file)
      (require (intern (file-name-sans-extension file)) nil t))))

;; Require all `.el' files in the `init.d' directory
(my-require-directory (expand-file-name "init.d" user-emacs-directory))

;; Require all `.el' files in the `packages.d' directory
(my-require-directory (expand-file-name "packages.d" user-emacs-directory))

;; Report the init load time
(add-hook 'after-init-hook
	  (lambda ()
	    (message "init completed in %.2fms"
		     (* 1000.0
			(float-time
			 (time-subtract after-init-time before-init-time))))))

(provide 'init)
