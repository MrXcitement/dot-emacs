;; init-multi-term.el

;; Initialize the multi-term mode that adds functionality to running a
;; shell in emacs.

;; Mike Barker <mike@thebarkers.com>
;; July 4th, 2010

;; TODO: Handle different shells for diffent hosts.

(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; With this, C-c t will jump through your multi-term buffers (create
;; a new one if it doesn not exist yet), while C-c T unconditionally
;; creates a new terminal
(global-set-key (kbd "C-c t") 'multi-term-next) ;; cycle terms
(global-set-key (kbd "C-c T") 'multi-term)	;; create a new one
