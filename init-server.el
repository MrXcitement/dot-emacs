;; init-server.el -- Initialize the server 

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Server mode

;; System type server settings
(cond
 ((eq system-type 'darwin)
  ;; Darwin (Mac OS X)
  )
 ((eq system-type 'gnu/linux)
  ;; Gnu/linux
  )
 ((eq system-type 'windows-nt)
  ;; Windows
  (setq server-auth-dir (getenv "TMP"))
  ))

;; Start a server for client processes, do not start if one is allredy running
(load "server")
(unless (server-running-p)
  (server-start))

(provide 'init-server)

