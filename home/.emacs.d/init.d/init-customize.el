;;; init-customize.el --- Configure the customize file

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Aquamacs needs it's own custom.el and some of the default settings in
;; custom.el will cause aquamacs to have problems. Also if Aquamacs
;; puts it's customize settings in the same file, Emacs may have
;; problems when it loads.

;;; History:
;; 2023.03.22
;; * refactor header to include standard sections.


;;; Code:
(if (featurep 'aquamacs)
    (setq custom-file
	  (expand-file-name "aquamacs.el" user-emacs-directory))
  (setq custom-file
	(expand-file-name "custom.el" user-emacs-directory)))
(load custom-file 'noerror)

(provide 'init-customize)
;;; init-customize.el ends here.
