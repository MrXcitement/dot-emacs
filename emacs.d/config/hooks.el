;;; hooks.el --- Initialize global hooks

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

(message "hooks -- Initialize global hooks...")

;;; Remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Highlight the current line when in dired mode.
(add-hook 'dired-mode-hook
	  (lambda() (hl-line-mode 1)))

;;; hook.el ends here
