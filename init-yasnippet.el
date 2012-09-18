;;;
;; init-yasnippet.el
;; load and initialize the yasnippet libaries

;; Mike Barker <mike@thebarkers.com>
;; Febuary 4th, 2010

;; See: http://code.google.com/p/yasnippet/

(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet-0.6.1c")
(when (require 'yasnippet)
  (yas-global-mode 1)
  ;; Add personal snippet directory and yasnippet directory
  ;;(add-to-list yas/root-directory '("~/.emacs.d/mysnippets"))
  (mapc 'yas/load-directory yas/root-directory))

(provide 'init-yasnippet)
