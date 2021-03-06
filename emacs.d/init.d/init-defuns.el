;;; init-defuns.el --- Personal defun declarations

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: January 29th 2015

;; This file is not part of GNU Emacs.

;;;;;;
;;; History

;; 2021-03-15 MRB
;; Removed copyright
;; Changed function name to use mrb/ instead of mrb: since the forward
;; slash seems to be the norm in personal elisp files.


;; In an effort to make my emacs configuration more modular I am using
;; the information found at the Emacs Wiki's Dot Emacs Modular article,
;; http://www.emacswiki.org/emacs-en/DotEmacsModular
;; I can now add initialization files configuration directory and they
;; will be loaded at startup.

;;; Load all .el files in a directory.
(defun mrb/load-directory (directory)
  "Load all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element))))
      (when (and (eq isdir nil) (string= (substring path -3) ".el"))
	(load (file-name-sans-extension fullpath))))))

(provide 'init-defuns)
;;; init-defuns.el ends here.
