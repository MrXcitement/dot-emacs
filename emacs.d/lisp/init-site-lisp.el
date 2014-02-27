;;; init-site-lisp.el -- Initialize the site-lisp directory

;; Mike Barker <mike@thebarkers.com>
;; March 2nd, 2013

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2013.03.02
;; * First release.

;; 2013.05.01
;; * This code was lifted from Steve Purcell's init-site-lisp at https://github.com/purcell/emacs.d

;; 2014.21.02
;; * Rename and cleanup of function names.
;; * Added code to ensure existing directories in the site-lisp directory are added to the
;;   load path so that we do not continue to download if the elisp is allready downloaded.

;; 2014-02-26 MRB
;; * BugFix: unable to download packages if the site-lisp directory
;;   did not exist.
;; * Cleanup the utility functions so that the path is only expanded
;;   when getting the user-emacs-directory.

;; 2014-02-27 MRB
;; * Some code/comments reformatting

;;; utility functions used to download elisp files.
(defun init:site-lisp-dir ()
  (expand-file-name "site-lisp" user-emacs-directory))

(defun init:site-lisp-dir-for (name)
  (format "%s/%s" (init:site-lisp-dir) name))

(defun init:site-lisp-library-el-path (name)
  (format "%s/%s.el" (init:site-lisp-dir-for name) name))

(defun init:site-lisp-download-module (name url)
  (let ((dir (init:site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (init:site-lisp-library-el-path name)))
      (unless (file-exists-p el-file)
	(url-copy-file url el-file t nil))
      el-file)))

(defun init:site-lisp-library-loadable-p (name)
  "Return whether or not the library 'name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (init:site-lisp-dir-for name)) f))))

(defun init:site-lisp-ensure-lib-from-url (name url)
  (unless (init:site-lisp-library-loadable-p name)
    (byte-compile-file (init:site-lisp-download-module name url))))

(defun init:add-subdirs-to-load-path (parent-dir)
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
(message "init-site-lisp -- Initialize the site-lisp packages directory...")

;;; Make sure the site-lisp directory exists
(unless (file-directory-p (init:site-lisp-dir))
  (make-directory (init:site-lisp-dir)))

;;; Add any pre-existing packages to the load path
(init:add-subdirs-to-load-path (init:site-lisp-dir))

;; package.el
;; http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el
;; allow emacs v23 to use the v24 package system.
(unless (> emacs-major-version 23)
  (init:site-lisp-ensure-lib-from-url 'package "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el")
  (require 'package))

;; powershell-mode.el
;; http://www.emacswiki.org/emacs/download/PowerShell-Mode.el
;; currently 8/2013 the version of this file on melpa is out of date.
(init:site-lisp-ensure-lib-from-url 'powershell-mode "http://www.emacswiki.org/emacs/download/PowerShell-Mode.el")
(autoload 'powershell-mode "powershell-mode" "Powershell mode." t)
(setq auto-mode-alist (append '(("\\.ps1$" .
				 powershell-mode)) auto-mode-alist))

;; visual-basic-mode.el
;; http://www.emacswiki.org/emacs/download/visual-basic-mode.el
(init:site-lisp-ensure-lib-from-url 'visual-basic-mode "http://www.emacswiki.org/emacs/download/visual-basic-mode.el")
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
				 visual-basic-mode)) auto-mode-alist))

(provide 'init-site-lisp)
