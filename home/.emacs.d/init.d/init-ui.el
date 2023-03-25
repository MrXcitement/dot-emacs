;;; init-ui.el --- Initialize the user interface

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Initialize the user interface handling text and gui modes.
;; System specific initialization is handled by the `init-ui-<system>.el' files.

;;; History:
;; 2023-03-25 MRB
;; * Move disabling UI elements to `early-init.el'
;; * Move configuring `dired' to `init-dired.el'
;; * Remove `load-theme' code.
;; * Move system specific configuration to `init-ui-<system>.el' files.
;; 2023-03-22 MRB
;; * Modify header to include standard sections.
;; 2021-03-15 MRB
;; Remove copyright
;; Only set font if it is available
;; 2014.11.12
;; * removed loading message

;;; Code:

;; Any ui settings
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)

;; Whitespace display configuration
(setq whitespace-line-column 80 whitespace-style
      '(face newline space-mark tab-mark newline-mark trailing lines-tail))

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

(provide 'init-ui)
;;; init-ui.el ends here.
