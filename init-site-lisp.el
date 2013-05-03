;;; init-site-lisp.el -- Initialize the site-lisp directory

;; Mike Barker <mike@thebarkers.com>
;; March 2nd, 2013

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2013.03.02
;; * First release.
;; 2013.05.01
;; * This code was lifted from Steve Purcell's init-site-lisp at https://github.com/purcell/emacs.d

(defun site-lisp-dir-for (name)
  (expand-file-name (format "~/.emacs.d/site-lisp/%s" name)))

(defun site-lisp-library-el-for (name)
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun site-lisp-library-elc-for (name)
  (expand-file-name (format "%s.elc" name) (site-lisp-dir-for name)))

(defun site-lisp-download-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (site-lisp-library-el-for name)))
      (unless (file-exists-p el-file)
	(url-copy-file url el-file t nil))
      el-file)))

(defun site-lisp-library-loadable-p (name)
  "Return whether or not the library 'name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))

(defun site-lisp-ensure-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (site-lisp-download-module name url))))

(defun site-lisp-ensure-lib-from-svn (name url)
  (let ((dir (site-lisp-dir-for name)))
    (unless (site-lisp-library-loadable-p name)
      (message "Checking out %s from svn" name)
      (save-excursion
	(shell-command (format "svn co %s %s" url dir) "*site-lisp-svn*"))
      (add-to-list 'load-path dir))))

;; "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el"
(defun site-lisp-ensure-libs ()
  (unless (> emacs-major-version 23)
    (site-lisp-ensure-lib-from-url 'package "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el")
    (require 'package)))

(site-lisp-ensure-libs)

(provide 'init-site-lisp)
