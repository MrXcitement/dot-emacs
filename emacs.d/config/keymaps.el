;;; keymaps.el --- Initialize global and system specific key maps

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

(message "keymaps -- Initialize global custom keymaps...")

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
  (message "...initialize darwin custom keymaps...")
  (global-set-key [kp-delete] 'delete-char)) ; Make fn-del delete forward

;;; Linux key mappings
(when (eq system-type 'linux)
  (message "...initialize linux custom keymaps..."))

;;; Windows key mappings
(when (eq system-type 'windows-nt)
  (message "...initialize windows custom keymaps..."))

;;; keymaps.el ends here.
