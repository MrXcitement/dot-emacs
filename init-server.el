;;;
;; init-server.el -- Initialize the emacs starting config and start it,
;;                   if it is not allready running.
;; Mike Barker <mike@thebarkers.com>
;; Copyright (C) 2011 Mike Barker, All rights reserved.
;; Updates:
;; 2012.10.15 MRB
;; * Fixed bug when running under Win32, I was setting the server-socket-dir
;;   and needed to set the server-auth-dir... It is now set correctly to the
;;   temp directory defined in the TMP environment variable. I also stopped
;;   putting it under a folder based on the user id, since I do not know this
;;   value when running emacsclient and would not be able to tell the client
;;   where to find the auth file.

;; Determine the system we are on
(cond

 ;; Darwin (Mac OS X) custimzations
 ((eq system-type 'darwin)
  )

 ;; Gnu/linux customizations
 ((eq system-type 'gnu/linux)
  )

 ;; Windows customizations
 ((eq system-type 'windows-nt)
  (setq server-auth-dir (getenv "TMP"))
 )
)
;;;
;; Start a server for client processes, do not start if one is allredy running
(load "server")
(unless (server-running-p)
  (server-start))

(provide 'init-server)

