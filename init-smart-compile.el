;;;
;; Configure smart-compile support
(setq load-path (append '("~/share/emacs/site-lisp/smart-compile") load-path))

(require 'smart-compile)
(global-set-key "C-cc" 'smart-compile)