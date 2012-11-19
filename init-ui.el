;; init-ui.el -- Initialize the user interface

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; User interface settings 
(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(tool-bar-mode -1)

;;; Window (gui) ui settings
(when (window-system)
  (load-theme 'tango t) ; Set the default color theme
  ;; System specific ui settings
  (cond
   ((eq system-type 'darwin)
    ;; Darwin (Mac OS X) gui custimzation
    (set-face-font 'default "Droid Sans Mono Slashed 14")
    )
   ((eq system-type 'gnu/linux)
    ;; Linux gui customization)
    )
   ((eq system-type 'windows-nt)
    ;; Windows customizations
    (set-face-font 'default "Lucida Console 10")
    )))

;;; Terminal ui settings
(unless (window-system)
  (menu-bar-mode -1))

(provide 'init-ui)
