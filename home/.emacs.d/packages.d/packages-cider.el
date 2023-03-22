;;; packages-cider.el --- Install and configure the `cider' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Clojure Interactive Development Environment that Rocks for Emacs,
;; built on top of nREPL, the Clojure networked REPL server.
;; https://github.com/clojure-emacs/cider

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2015.05.15
;; * First release.

;;; Code:
(when (executable-find "lien")
  (use-package cider))

(provide 'packages-cider)
