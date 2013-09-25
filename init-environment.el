;; init-environment.el -- Initialize the environment

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012-11-18
;; * First release.

;; 2012-11-24 MRB
;; * No longer set whitespace here, should be set per mode.

;; 2013-01-09 MRB
;; * Removed path to macports bin (/opt/local/bin)

;; 2013-09-25
;; System specific environement settings moved to init-environment-<system>
;; where <system> is: darwin, gnu-linux, windows-nt

(cond
 ((string-equal "darwin" system-type)
  ;; configure mac os x configuration
  (require 'init-environment-darwin nil t))

 ((string-equal "gnu/linux" system-type)
  ;; configure linux environment
  )
 ((string-equal "windows-nt" system-type )
  ;; configure windows environment
  ))

(provide 'init-environment)
