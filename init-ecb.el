;;;
;; init-ecb.el
;; Initialize the Emacs Code Browser system

;; Mike Barker
;; May 21st, 2009

;; See: http://www.emacswiki.org/emacs/EmacsCodeBrowser

;; ECB requires the CEDET system
(require 'init-cedet)

;; Initialize the ECB system
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-snap/")
(setq stack-trace-on-error t)
(require 'ecb)
;; (ecb-activate)
(set-face-foreground (quote ecb-default-highlight-face) "DimGray")
(set-face-background (quote ecb-default-highlight-face) "grey60")

(provide 'init-ecb)
