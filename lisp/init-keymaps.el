;; init-keymaps.el -- Initialize global and system specific key maps

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Key mappings

;; Compilation output, next/previous error. (Alt-Page Up/Alt-Page Down)
(global-set-key (kbd "<M-prior>") 'previous-error)
(global-set-key (kbd "<M-next>")  'next-error)

;; System specific key mappings
(cond
 ;; Darwin (Mac OS X) key mappings
 ((string-equal "darwin" system-type)
  (global-set-key [kp-delete] 'delete-char) ; Make fn-del delete forward
  )
 ((string-equal "gnu/linux" system-type)
  ;; Linux key mappings
  )
 ((string-equal "windows-nt" system-type )
  ;; Windows key mappings
  ))

(provide 'init-keymaps)
