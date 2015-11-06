;;; init-keymaps.el --- Initialize global and system specific key maps

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message


;; Configure cua mode to allow selection of text only.
;; This allows the C-x,c,v keys to retain their original functionality
;; but allow cua rectangle selection.
(cua-selection-mode 1)

;;; Compilation output, next/previous error. (<alt-{page up/page down}>)
(global-set-key (kbd "<M-prior>") 'previous-error)
(global-set-key (kbd "<M-next>")  'next-error)

;;; Move to window support (<C-c-{up,down,left,right}>)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; Darwin (Mac OS X) key mappings
(when (eq system-type 'darwin)
  (global-set-key [kp-delete] 'delete-char)       ; Make fn-del delete forward
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0))))

;;; Linux key mappings
(when (eq system-type 'linux))

;;; Windows key mappings
(when (eq system-type 'windows-nt))

(provide 'init-keymaps)
;;; init-keymaps.el ends here.
