;;; init-ui-windows.el --- initialze the user interface on Windows systems

;; Mike Barker <mike@thebarkers.com>
;; March 25th, 2023

;;; Commentary:
;; Initialize the user interface on Windows systems

;;; History:
;; 2023-03-26 MRB
;; * Handle Emacs being run as a daemon and normally
;; 2023-03-25 MRB
;; * Created

;;; Code:

;; Windows customization
(when (eq system-type 'windows-nt)

  ;; Frame configuration for `windows' systems.
  (defun my-after-make-frame-windows(&optional frame)
    "Configure a new FRAME (default: selected frame) on `windows' system"

    ;; When the frame is GUI
    (when (display-graphic-p)

      ;; Font customization
      (when (member "Lucida Console" (font-family-list))
	(set-face-font 'default "Lucida Console 10"))))

  ;; Hook make frame to apply `windows' specific configuration
  (add-hook 'after-make-frame-functions 'my-after-make-frame-windows)

  ;; Emacs not started in `daemon' mode.
  (unless (daemonp)
    (my-after-make-frame-windows)))

(provide 'init-ui-windows)
;;; End of init-ui-windows
