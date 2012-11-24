;; init-environment.el -- Initialize the environment

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;; 2012.11.24 MRB
;; * No longer set whitespace here, should be set per mode.

;; if the user name is not set, set it
(when (eq user-full-name "")
    (setq user-full-name "Mike Barker"))

;; if the email is not set, set it
(when (eq user-mail-address "")
    (setq user-mail-address "mike@thebarkers.com"))

;;; Whitespace configuration
;; (setq-default show-trailing-whitespace t)
;; (setq indicate-empty-lines t)
;; Remove trailing whitespace when saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; System specific environement settings
(cond
 ((string-equal "darwin" system-type)
  ;; configure mac os x configuration
  (let ((mypaths '("/Users/mike/bin"
		   "/Users/Shared/bin"
		   "/Library/Frameworks/Python.framework/Versions/Current/bin"
		   "/opt/local/bin"
		   "/usr/local/git/bin"
		   "/usr/local/bin"
		   "/usr/X11/bin"
		   "/usr/bin"
		   "/bin")))
    (setenv "PATH" (mapconcat 'identity mypaths ":"))
    (setq exec-path mypaths))
  )
 ((string-equal "gnu/linux" system-type)
  ;; configure linux environment
  )
 ((string-equal "windows-nt" system-type )
  ;; configure windows environment
  ))

(provide 'init-environment)
