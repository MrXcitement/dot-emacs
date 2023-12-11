;;; init-save-backup.el --- Initialize save, autosave and backup settings

;; Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;;; Commentary:
;; Initialize the save, autosave and backup of files

;;; History:
;; 2023.03.22
;; * modify header to include standard sections.
;; 2014.11.12
;; * removed loading message

;;; Code:

;; Remove trailing whitespace from lines when saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set the temp directory to be a directory in the users home
;; directory. ~/tmp/emacs
(let ((mrb-temp-directory (expand-file-name "~/tmp/emacs/")))

  ;; make the temp directory
  (make-directory mrb-temp-directory t)

  ;; Backup files to the temp directory
  (setq backup-by-copying t)
  (setq backup-directory-alist
      `((".*" . ,mrb-temp-directory)
        (,tramp-file-name-regexp nil)))

  ;; Auto save files to the temp directory
  (setq auto-save-list-file-prefix
	(concat mrb-temp-directory "auto-saves-"))

  (setq auto-save-file-name-transforms
	`(("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,mrb-temp-directory t))))

(provide 'init-save-backup)
;;; init-save-backup.el ends here.
