;;; init-ui.el --- Initialize the user interface

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Initialize the user interface no handling text and gui and
;; `darwin', `linux' and `windows' systems.

;;; History:
;; 2023-03-22 MRB
;; * Mmodify header to include standard sections.
;; 2021-03-15 MRB
;; Remove copyright
;; Only set font if it is available
;; 2014.11.12
;; * removed loading message

;;; Code:

;; Any ui settings
(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(tool-bar-mode -1)

;; Whitespace display configuration
(setq whitespace-line-column 80 whitespace-style
      '(face newline space-mark tab-mark newline-mark trailing lines-tail))

;; Default theme
;(load-theme 'tango-dark)

;; Any GUI configuration
(when (window-system)

  ;; Switch theme based on system appearance
  (defun my-apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'vs-light t))
      ('dark (load-theme 'vs-dark t))))

  ;; Toggle fullscreen
  (defun my-toggle-fullscreen ()
    "Toggle full screen"
    (interactive)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

  (global-set-key [f11] 'my-toggle-fullscreen))

;; Any TUI settings
(unless (window-system)
  (menu-bar-mode -1))

;; Darwin (Mac OS X) customization
(when (eq system-type 'darwin)

  ;; GUI Configuration
  (when (window-system)

    ;; Raise emacs to frontmost window
    (when (featurep 'ns)
      (defun ns-raise-emacs ()
	"Raise Emacs."
	(ns-do-applescript "tell application \"Emacs\" to activate"))
      (defun ns-raise-emacs-with-frame (frame)
	"Raise Emacs and select the provided frame."
	(with-selected-frame frame
	  (when (display-graphic-p)
	    (ns-raise-emacs))))
      (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
      (when (display-graphic-p)
	(ns-raise-emacs)))

    ;; hook the dark/light theme switcher
    (add-hook 'ns-system-appearance-change-functions #'my-apply-theme)

    ;; set key to toggle fullscreen mode
    (global-set-key (kbd "s-<return>") 'my-toggle-fullscreen)

    ;; set default font
    (when (member "FiraCode Nerd Font" (font-family-list))
      (set-frame-font "FiraCode Nerd Font" t t))))

;; Linux customization
(when (eq system-type 'gnu/linux)
  (when (window-system)
    (when (member "Monospace" (font-family-list))
      (set-face-font 'default "Monospace 11"))))

;; Windows customizations
(when (eq system-type 'windows-nt)
  (when (window-system)
    (when (member "Lucida Console" (font-family-list))
      (set-face-font 'default "Lucida Console 10"))))

(provide 'init-ui)
;;; init-ui.el ends here.
