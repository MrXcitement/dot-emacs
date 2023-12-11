;;; init-ui-linux.el --- initialze the user interface on Linux systems

;; Mike Barker <mike@thebarkers.com>
;; March 25th, 2023

;;; Commentary:
;; Initialize the user interface on Linux systems

;;; History:
;; 2023-03-26 MRB
;; * Handle Emacs being run as a daemon and normally
;; 2023-03-25 MRB
;; * Created

;;; Code:

;; Linux customization
(when (eq system-type 'gnu/linux)

  ;; Frame configuration for `windows' systems.
  (defun mrb-after-make-frame-linux(&optional frame)
    "Configure a new FRAME (default: selected frame) on `linux' system"

    ;; When the frame is GUI
    (when (display-graphic-p)

      ;; Font customization
      (when (member "Monospace" (font-family-list))
	(set-face-font 'default "Monospace 11"))))

  ;; Hook make frame to apply `linux' specific configuration
  (add-hook 'after-make-frame-functions 'mrb-after-make-frame-linux)

  ;; Emacs not started in `daemon' mode.
  (unless (daemonp)
    (mrb-after-make-frame-linux)))

(provide 'init-ui-linux)
;;; End of init-ui-linux
