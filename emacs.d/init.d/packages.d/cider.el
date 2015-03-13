;;; cider:

;; Clojure Interactive Development Environment that Rocks for Emacs,
;; built on top of nREPL, the Clojure networked REPL server.
;; https://github.com/clojure-emacs/cider

;; When the leim command is available, install/config cider
(when (mrb:eshell-command-exist-p "lein")
  (package-install 'cider))

(use-package cider)
