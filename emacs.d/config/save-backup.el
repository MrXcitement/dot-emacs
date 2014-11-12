;;; save-backup.el --- Initialize autosave and backup settings

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com> Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message

;;; Set the temp directory to be a directory in the users home
;;; directory. ~/tmp/emacs
(let ((user-temp-directory (expand-file-name "~/tmp/emacs/")))

  ;; make the temp directory
  (make-directory user-temp-directory t)

  ;; Backup files to the temp directory
  (setq backup-by-copying t)
  (setq backup-directory-alist
      `((".*" . ,user-temp-directory)
        (,tramp-file-name-regexp nil)))

  ;; Auto save files to the temp directory
  (setq auto-save-list-file-prefix
	(concat user-temp-directory "auto-saves-"))

  (setq auto-save-file-name-transforms
	`(("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,user-temp-directory t))))

;;; save-backup.el ends here.
