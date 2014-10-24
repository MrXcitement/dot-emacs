;;; zz-server.el --- Initialize the server

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

(message "server -- Initialize the emacs server process...")

;;; Darwin (Mac OS X)
(when (eq system-type 'darwin)
  (message "...darwin settings for the  server process..."))

;;; Gnu/linux
(when (eq system-type 'gnu/linux)
  (message "...darwin settings for the  server process..."))

;;; Windows
(when (eq system-type 'windows-nt)
  (message "...darwin settings for the  server process...")
  (setq server-auth-dir (getenv "TMP")))

;;; Start a server for client processes, but only if one is not already running
(load "server")
(unless (server-running-p)
  (server-start))

;;; zz-server.el ends here.
