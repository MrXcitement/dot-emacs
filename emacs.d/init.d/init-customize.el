;;; init-customize.el --- Configure the customize file

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;; Aquamacs needs it's own custom.el and some of the default settings in
;; custom.el will cause aquamacs to have problems. Also if Aquamacs
;; puts it's customize settings in the same file, Emacs may have
;; problems when it loads.
(if (featurep 'aquamacs)
    (setq custom-file
	  (expand-file-name
	   (concat user-emacs-directory "/aquamacs.el")))

  (setq custom-file
	(expand-file-name
	 (concat user-emacs-directory "/custom.el"))))

(load custom-file 'noerror)

(provide 'init-customize)
;;; customize.el ends here.
