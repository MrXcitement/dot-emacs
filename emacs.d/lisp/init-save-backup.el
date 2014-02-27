;; init-save-backup.el -- Initialize autosave and backup settings

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012.11.18
;; * First release.

;; 2014-02-27 MRB
;; * Added initialization message.
;; * rename variables to use init: prefix

;;; Autosave and backup support
(message "init-save-backup -- Initialize auto save and backup modes...")

;; Set the temp directory location, makeing it if needed.
(setq init:user-temp-directory (expand-file-name "~/tmp/emacs/"))
(make-directory init:user-temp-directory t)

;; Backup files to the temp directory
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,init:user-temp-directory)
        (,tramp-file-name-regexp nil)))

;; Auto save files to the temp directory
(setq auto-save-list-file-prefix
      (concat init:user-temp-directory "auto-saves-"))
(setq auto-save-file-name-transforms
      `(("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,init:user-temp-directory t)))

(provide 'init-save-backup)
