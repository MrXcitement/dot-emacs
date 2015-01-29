;;; zz-server.el --- Initialize the server

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message

;;; Darwin (Mac OS X)
(when (eq system-type 'darwin))

;;; Gnu/linux
(when (eq system-type 'gnu/linux))

;;; Windows
(when (eq system-type 'windows-nt)
  (setq server-auth-dir (getenv "TMP")))

;;; Start a server for client processes, but only if one is not already running
(load "server")
(unless (server-running-p)
  (server-start))

;;; zz-server.el ends here.
