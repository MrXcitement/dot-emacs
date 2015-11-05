;;; init-defuns.el --- Personal defun declarations

;; Copyright (C) 2015 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: January 29th 2015

;; This file is not part of GNU Emacs.


;;; Load a all .el files in a directory and it's subdirectory.

;; In an effort to make my emacs configuration more modular I am using
;; the information found at the Emacs Wiki's Dot Emacs Modular article,
;; http://www.emacswiki.org/emacs-en/DotEmacsModular
;; I can now add initialization files configuration directory and they
;; will be loaded at startup.

;; TODO: Add the capability to byte compile updated files and load the
;; byte-compiled file

(defun mrb:load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (mrb:load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))


;;; Eshell functions
;; I use the internal eshell to allow cross platform shell script
(defun mrb:eshell-command-to_string (command)
  "Run the eshell command specified and return the results as a string."
  (with-temp-buffer
    (eshell-command command t)
    (buffer-string)))

(defun mrb:eshell-command-exist-p (command)
  "Does a specified command exist on this machine."
  (not (string-equal (mrb:eshell-command-to_string (concat "which " command)) "")))

(provide 'init-defuns)

;;; init-defuns.el ends here.
