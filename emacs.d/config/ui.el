;;; ui.el --- Initialize the user interface

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

(message "ui -- Initialize the user interface...")

(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(tool-bar-mode -1)

;; Configure cua mode to allow selection of text only.
;; This allows the C-x,c,v keys to retain their original functionality
;; but allow cua rectangle selection.
(cua-selection-mode 1)

;;; Whitespace display configuration
(setq whitespace-line-column 80 whitespace-style
      '(face newline space-mark tab-mark newline-mark trailing lines-tail))

;;; Window (gui) ui settings
(when (window-system)

  ;; Any window-system
  (defun toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
  (global-set-key [f11] 'toggle-fullscreen)

  ;; Darwin (Mac OS X) customization
  (when (eq system-type 'darwin)
    (set-face-font 'default "Droid Sans Mono Slashed 14")
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)
    (global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0))))

  ;; Linux customization
  (when (eq system-type 'gnu/linux)
    (set-face-font 'default "Monospace 11"))

  ;; Windows customizations
  (when (eq system-type 'windows-nt)
    (set-face-font 'default "Lucida Console 10")))

;;; Terminal ui settings
(unless (window-system)
  (menu-bar-mode -1)
  ;; on xterm's remap the select key to shift-up
  (if (string-match-p "xterm" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up])))

;;; ui.el ends here.
