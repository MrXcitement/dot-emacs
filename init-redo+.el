;;;
;; init-redo+.el -- emacswiki
(message "Loading init-redo+.el...")

(require 'redo+)
;; In addition, if you don't want to redo a previous undo, add
;;(setq undo-no-redo t)

;; Set keyboard shortcuts
(global-set-key (kbd "C-?") 'redo)   ; [Ctrl+Shift+/]
(global-set-key (kbd "C-x r") 'redo) ; [Ctrl+x r]

(provide 'init-redo+)
