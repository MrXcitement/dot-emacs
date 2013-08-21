;; init-environment.el -- Initialize the environment

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;; 2012.11.24 MRB
;; * No longer set whitespace here, should be set per mode.

;; 2013.01.09 MRB
;; * Removed path to macports bin (/opt/local/bin)

;; System specific environement settings
(cond
 ((string-equal "darwin" system-type)
  ;; configure mac os x configuration
  (let ((mypaths '("~/bin"
		   "/Users/Shared/bin"
		   "/usr/local/share/python"
		   "/usr/local/git/bin"
		   "/usr/local/bin"
		   "/usr/local/sbin"
		   "/usr/X11/bin"
		   "/usr/bin"
		   "/usr/sbin"
		   "/bin"
		   "/sbin")))
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
