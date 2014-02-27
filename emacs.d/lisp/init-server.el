;; init-server.el -- Initialize the server

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012.11.18
;; * First release.

;; 2014-02-27 MRB
;; * Added initialization message
;; * Some code/comment reformatting

;;; Server mode
(message "init-server -- Initialize the emacs server process...")

;; System type server settings
(cond
 ;; Darwin (Mac OS X)
 ((eq system-type 'darwin)
  )
 ;; Gnu/linux
 ((eq system-type 'gnu/linux)
  )
 ;; Windows
 ((eq system-type 'windows-nt)
  (setq server-auth-dir (getenv "TMP"))
  ))

;; Start a server for client processes, but only if one is not already running
(load "server")
(unless (server-running-p)
  (server-start))

(provide 'init-server)
