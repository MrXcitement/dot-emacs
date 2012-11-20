;; init-save-backup.el -- Initialize autosave and backup settings

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Autosave and backup support

;; Set the temp directory location, makeing it if needed.
(setq user-temporary-file-directory (expand-file-name "~/tmp/emacs/"))
(make-directory user-temporary-file-directory t)

;; Backup files to the temp directory
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

;; Auto save files to the temp directory
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory "auto-saves-"))
(setq auto-save-file-name-transforms
      `(("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,user-temporary-file-directory t)))

(provide 'init-save-backup)
