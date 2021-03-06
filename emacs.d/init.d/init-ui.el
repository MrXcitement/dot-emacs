;;; init-ui.el --- Initialize the user interface

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message

;; 2021-03-15 MRB
;; Remove copyright
;; Only set font if it is available

;;; General ui settings
(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(tool-bar-mode -1)

;;; Whitespace display configuration
(setq whitespace-line-column 80 whitespace-style
      '(face newline space-mark tab-mark newline-mark trailing lines-tail))

;;; Highlight the current line when in dired mode.
(add-hook 'dired-mode-hook
	  (lambda() (hl-line-mode 1)))

;;; Default theme
;;(load-theme 'tango-dark)


;;; Window (gui) ui settings
(when (window-system)

  ;; Any window-system
  (defun mrb:toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
  (global-set-key [f11] 'mrb:toggle-fullscreen)

  ;; Darwin (Mac OS X) customization
  (when (eq system-type 'darwin)
    (global-set-key (kbd "s-<return>") 'mrb:toggle-fullscreen)
    (when (member "FiraCode Nerd Font" (font-family-list))
      (set-frame-font "FiraCode Nerd Font" t t)))

  ;; Linux customization
  (when (eq system-type 'gnu/linux)
    (when (member "Monospace" (font-family-list))
      (set-face-font 'default "Monospace 11")))

  ;; Windows customizations
  (when (eq system-type 'windows-nt)
    (when (member "Lucida Console" (font-family-list))
      (set-face-font 'default "Lucida Console 10")))
)


;;; Terminal ui settings
(unless (window-system)
  (menu-bar-mode -1)
  ;; on xterm's remap the select key to shift-up
  (if (string-match-p "xterm" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up])))

(provide 'init-ui)
;;; init-ui.el ends here.
