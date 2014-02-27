;; init-hooks.el -- Initialize global hooks

;; Mike Barker <mike@thebarkers.com>
;; February 27, 2014

;; Copyright (c) 2014 Mike Barker

;; Change log:
;; 2014.02.27
;; * First release.

(message "init-hooks -- Initialize global hooks...")

;; Remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight the current line when in dired mode.
(add-hook 'dired-mode-hook
	  (lambda() (hl-line-mode 1)))

(provide 'init-hooks)
