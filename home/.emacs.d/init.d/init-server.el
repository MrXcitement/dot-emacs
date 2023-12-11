;;; init-server.el --- Initialize the server

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Start the server whenever the main emacs app is run as a `gui' If
;; you need to make any system specific settings for the server to
;; run, make them in the provided system sections.

;;; History:
;; 2023.03.22
;; * modify header to include standard sections.
;; 2014.11.12
;; * removed loading message

;;; Code:

;; Darwin (Mac OS X)
(when (eq system-type 'darwin))

;; Gnu/linux
(when (eq system-type 'gnu/linux))

;; Windows
(when (eq system-type 'windows-nt)
  (setq server-auth-dir (getenv "TMP")))

;; When running as a GUI
;; Start a server for client processes, but only if one is not already running
(when (window-system)
  (load "server")
  (unless (server-running-p)
    (server-start)))

(provide 'init-server)
;;; init-server.el ends here.
