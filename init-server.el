;;;
;; init-server.el -- Initialize the emacs starting config and start it,
;;                   if it is not allready running.
;; Mike Barker <mike@thebarkers.com>
;; Copyright (C) 2011 Mike Barker, All rights reserved.

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
  (setq server-socket-dir (format "%s\\%d" (getenv "TMP")(user-uid)))
  )
 )

;;;
;; Start a server for client processes, do not start if one is allredy running
(load "server")
(unless (server-running-p)
  (server-start))

(provide 'init-server)

