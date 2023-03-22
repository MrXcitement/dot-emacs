;;; init-keymaps.el --- Initialize global and system specific key maps

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Put keymaps that should be available in any emacs session regardless
;; of the system in the `Global' section. Any system specific ones go into
;; `darwin', `linux' or `windows-nt' sections.

;;; History:
;; 2023.03.22
;; * modify header to include default sections.
;; 2014.11.12
;; * removed loading message

;;; Code:

;;; Global key mappings
;; Configure cua mode to allow selection of text only.
;; This allows the C-x,c,v keys to retain their original functionality
;; but allow cua rectangle selection.
(cua-selection-mode 1)

;; Compilation output, next/previous error. (<alt-{page up/page down}>)
(global-set-key (kbd "<M-prior>") 'previous-error)
(global-set-key (kbd "<M-next>")  'next-error)

;; Move to window support (<C-c-{up,down,left,right}>)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Darwin (Mac OS X) key mappings
(when (eq system-type 'darwin)
  (global-set-key [kp-delete] 'delete-char)       ; Make fn-del delete forward
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0))))

;; Linux key mappings
(when (eq system-type 'linux))

;; Windows key mappings
(when (eq system-type 'windows-nt))

(provide 'init-keymaps)
;;; init-keymaps.el ends here.
