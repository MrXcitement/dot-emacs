;;; site-lisp.el --- Initialize the site-lisp directory

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message

;;; utility functions used to download elisp files.
(defun mrb:site-lisp-dir ()
  (expand-file-name "site-lisp" user-emacs-directory))

(defun mrb:site-lisp-dir-for (name)
  (format "%s/%s" (mrb:site-lisp-dir) name))

(defun mrb:site-lisp-library-el-path (name)
  (format "%s/%s.el" (mrb:site-lisp-dir-for name) name))

(defun mrb:site-lisp-download-module (name url)
  (let ((dir (mrb:site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (mrb:site-lisp-library-el-path name)))
      (unless (file-exists-p el-file)
	(url-copy-file url el-file t nil))
      el-file)))

(defun mrb:site-lisp-library-loadable-p (name)
  "Return whether or not the library 'name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (mrb:site-lisp-dir-for name)) f))))

(defun mrb:site-lisp-ensure-lib-from-url (name url)
  (unless (mrb:site-lisp-library-loadable-p name)
    (byte-compile-file (mrb:site-lisp-download-module name url))))

(defun mrb:add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (string-match "^\\." dir)
                   collecting (expand-file-name dir))
             load-path)))))

;;; ensure that third party packages are downloaded and then loaded
;;; from the site-lisp subdirectory.

;;; Make sure the site-lisp directory exists
(unless (file-directory-p (mrb:site-lisp-dir))
  (make-directory (mrb:site-lisp-dir)))

;;; Add any pre-existing packages to the load path
(mrb:add-subdirs-to-load-path (mrb:site-lisp-dir))

;;; site-lisp.el ends here.
