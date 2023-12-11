;;; init-ui-any.el --- Initialize the user interface, any system

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

;; Apply theme based on system appearance
(defun mrb-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'vs-light t))
    ('dark (load-theme 'vs-dark t))))

;; Apply light theme
(defun mrb-apply-theme-light ()
  "Apply the light theme"
  (interactive)
  (mrb-apply-theme 'light))

;; Apply dark theme
(defun mrb-apply-theme-dark ()
  "Apply the dark theme"
  (interactive)
  (mrb-apply-theme 'dark))

;; Any GUI/TUI configuration
(defun mrb-any-after-make-frame (&optional frame)
  "Configure a new FRAME (default: selected frame) on any system"

  ;; Display the menubar in GUI and hide in TUI frames
  (let ((lines (if (display-graphic-p frame) 1 0)))
    (set-frame-parameter frame 'menu-bar-lines lines)))

;; Add hook to configure new frames either GUI or TUI
(add-hook 'after-make-frame-functions 'mrb-any-after-make-frame)

;; Emacs was started normally
(unless (daemonp)
  (mrb-any-after-make-frame))

(provide 'init-ui-any)
;;; init-ui.el ends here.
