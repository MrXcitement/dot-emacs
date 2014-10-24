;;; magit.el --- initialize the magit package
(mrb:package-install 'magit)
(eval-after-load 'magit-autoloads
    (progn
      (require 'magit nil t)

      ;; show full screen magit-status
      (defadvice magit-status (around magit-fullscreen activate)
	(window-configuration-to-register :magit-fullscreen)
	ad-do-it
	(delete-other-windows))

      ;; restore windows when quit magit-status
      (defun magit-quit-session ()
	"Restore the previous window configuration and kills the magit buffer"
	(interactive)
	(kill-buffer)
	(jump-to-register :magit-fullscreen))

      (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

      (defun magit-toggle-whitespace ()
	(interactive)
	(if (member "-w" magit-diff-options)
	    (magit-dont-ignore-whitespace)
	  (magit-ignore-whitespace)))

      (defun magit-ignore-whitespace ()
	(interactive)
	(add-to-list 'magit-diff-options "-w")
	(magit-refresh))

      (defun magit-dont-ignore-whitespace ()
	(interactive)
	(setq magit-diff-options (remove "-w" magit-diff-options))
	(magit-refresh))

      (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))
