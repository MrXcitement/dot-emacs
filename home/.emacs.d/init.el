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
;; 2023-03-25
;; * modify Emacs version is earlier than v24 check.
;; * add Emacs version is earlier than v27 check.
;; * move the init load time report to `early-init.el'
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

;; Emacs version earlier than v24
(let ((minver 26))
  (when (< emacs-major-version minver)
    (error "Your Emacs v%s is too old -- this config requires v%s or higher" emacs-version minver)))

;; Emacs version earlier than v27
(let ((minver 27))
  (when (< emacs-major-version minver)
    (progn
      (message "Your Emacs v%s is old -- this configuration may not work as expected since Emacs is < v%s." emacs-version minver)
      (load-file "early-init.el"))))

;; Require all `.el' files in a directory
(defun mrb-require-directory (directory)
  "Require all `.el' files in DIRECTORY."
  (when (file-exists-p directory)
    (add-to-list 'load-path directory)
    (dolist (file (directory-files directory nil "\\.el$"))
      (message "require file %s..." file)
      (require (intern (file-name-sans-extension file)) nil t))))

;; Require all `.el' files in the `init.d' directory
(mrb-require-directory (expand-file-name "init.d" user-emacs-directory))

;; Require all `.el' files in the `packages.d' directory
(mrb-require-directory (expand-file-name "packages.d" user-emacs-directory))

(provide 'init)
