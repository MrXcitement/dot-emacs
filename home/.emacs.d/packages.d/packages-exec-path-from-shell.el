;;; packages-exec-path-from-shell.el --- Install and configure the `exec-path-from-shell' package.

;; Mike Barker <mike@thebarkers.com>
;; March 16th, 2023

;;; Commentary:
;; When on a Darwin (macOS) system
;; copy important environment variables from the user's shell
;; by asking your shell to print out the variables of interest,
;; then copying them into the Emacs environment.
;; https://github.com/purcell/exec-path-from-shell

;;; History:
;; 2023-03-16
;; * First release.

;;; Code:
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(provide 'packages-exec-path-from-shell)
