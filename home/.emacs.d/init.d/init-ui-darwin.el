;;; init-ui-darwin.el --- initialze the user interface on Darwin (macOS) systems

;; Mike Barker <mike@thebarkers.com>
;; March 25th, 2023

;;; Commentary:
;; Initialize the user interface on Darwin systems

;;; History:
;; 2023-03-25 MRB
;; * Created

;;; Code:

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

(provide 'init-ui-darwin)
;;; End of init-ui-darwin.el
