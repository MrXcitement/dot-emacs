;; init-ui.el -- Initialize the user interface

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012.11.18
;; * First release.

;; 2013-05-28 MRB
;; * No longer loading a theme
;; * Removed whitespace configuration

;; 2013-08-23 MRB
;; * Set the 'move to window' support to use the meta key
;; * Set cua selection rectangle mode -- C-Return to enable/cancel

;; 2014-02-11 MRB
;; * Clean up the whitespace settings.

;; 2014-02-27 MRB
;; * Added initialization message
;; * Minor code/comment reformatting

;;; User interface settings

(message "init-ui -- Initialize the user interface...")

(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(tool-bar-mode -1)

;; turn on move to window support (<C-c-{up,down,left,right}>)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Configure cua mode to allow selection of text only.
;; This allows the C-x,c,v keys to retain their original funcitonality
;; but allow cua rectangle selection.
(cua-selection-mode 1)

;;; Whitespace configuration
(setq whitespace-line-column 80 whitespace-style
      '(face newline space-mark tab-mark newline-mark trailing lines-tail))

;; activate minor whitespace mode when in python mode
;;(add-hook 'python-mode-hook 'whitespace-mode)

;;; Window (gui) ui settings
(when (window-system)

  ;; any window-system
  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
  (global-set-key [f11] 'toggle-fullscreen)

  ;; System specific ui settings
  (cond

   ;; Darwin (Mac OS X) customization
   ((eq system-type 'darwin)
    (set-face-font 'default "Droid Sans Mono Slashed 14")
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)
    (global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0))))

   ;; Linux customization
   ((eq system-type 'gnu/linux)
    (set-face-font 'default "Monospace 11"))

   ;; Windows customizations
   ((eq system-type 'windows-nt)
    (set-face-font 'default "Lucida Console 10"))))

;;; Terminal ui settings
(unless (window-system)
  (menu-bar-mode -1)
  ;; on xterm's remap the select key to shift-up
  (if (string-match-p "xterm" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up])))

(provide 'init-ui)
