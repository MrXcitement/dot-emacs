;;;
;; init-icicles.el
;; Initialize the Icicles command completion

;; Mike Barker <mike@thebarkers.com>
;; May 21st, 2009

;; See: http://www.emacswiki.org/emacs/Icicles

;; Only configure icicles on windowing systems
(if (not(eq window-system nil))
    (progn
      (require 'icicles)
      (icy-mode 1)
      ))					

(provide 'init-icicles)