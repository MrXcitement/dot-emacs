;; init-environment-darwin.el -- Initialize the environment when running under darwin/mac osx

;; Mike Barker <mike@thebarkers.com>
;; September 25, 2013

;; Copyright (c) 2013 Mike Barker

;; Change log:
;; 2013.09.25
;; * First release.

;; 2014.02.11
;; * Now set the current directory to the users home directory at startup

;; 2014-02-27 MRB
;; * Added initialization message

(message "Initializing the darwin environment (init-environment-darwin)...")

;; Needed for hunspell to work properly
(setenv "DICTIONARY" "en_US")

;; Setup the path.
(let ((mypaths '("~/bin"
		 "/Users/Shared/bin"
		 "/usr/local/share/python"
		 "/usr/local/git/bin"
		 "/usr/local/bin"
		 "/usr/local/sbin"
		 "/usr/bin"
		 "/usr/sbin"
		 "/bin"
		 "/sbin")))
  (setenv "PATH" (mapconcat 'identity mypaths ":"))
  (setq exec-path mypaths))

;; Force the current directory to be the users home dir
(setq default-directory "~/")

(provide 'init-environment-darwin)
