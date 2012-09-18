;;;
;; Protect buffers from being killed
(message "Loading init-protbuf.el...")
(require 'protbuf)
(protect-buffer-from-kill-mode nil (get-buffer "*scratch*"))
(protect-buffer-from-kill-mode nil (get-buffer "*Messages*"))

(provide 'init-protbuf)
