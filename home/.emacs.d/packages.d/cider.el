;;; cider:

;; Clojure Interactive Development Environment that Rocks for Emacs,
;; built on top of nREPL, the Clojure networked REPL server.
;; https://github.com/clojure-emacs/cider

(when (executable-find "lien")
  (use-package cider))
