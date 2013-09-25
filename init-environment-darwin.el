;; init-environment-darwin.el -- Initialize the environment when running under darwin/mac osx

;; Mike Barker <mike@thebarkers.com>
;; September 25, 2013

;; Copyright (c) 2013 Mike Barker

;; Change log:
;; 2013.09.25
;; * First release.

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

(provide 'init-environment-darwin)
