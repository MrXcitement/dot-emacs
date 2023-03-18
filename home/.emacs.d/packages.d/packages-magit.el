;;; packages-magit.el --- Install and configure the `magit' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Magit is an interface to the version control system Git,
;; implemented as an Emacs package. Magit aspires to be a complete Git
;; porcelain. While we cannot (yet) claim that Magit wraps and
;; improves upon each and every Git command, it is complete enough to
;; allow even experienced Git users to perform almost all of their
;; daily version control tasks directly from within Emacs. While many
;; fine Git clients exist, only Magit and Git itself deserve to be
;; called porcelains.
;; https://www.emacswiki.org/emacs/Magit

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2015.05.15
;; * First release.

;;; Code:
(use-package magit
  :ensure t)
  ;; :config
  ;; (progn
    ;; show full screen magit-status
    ;; (defadvice magit-status (around magit-fullscreen activate)
    ;; 	(window-configuration-to-register :magit-fullscreen)
    ;; 	ad-do-it
    ;; 	(delete-other-windows))

    ;; restore windows when quit magit-status
    ;; (defun magit-quit-session ()
    ;; 	"Restore the previous window configuration and kills the magit buffer"
    ;; 	(interactive)
    ;; 	(kill-buffer)
    ;; 	(jump-to-register :magit-fullscreen))

    ;; use q to quit magit session
    ;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    ;; cycle through whitspace handling
    ;; (defun magit-toggle-whitespace ()
    ;; 	(interactive)
    ;; 	(if (member "-w" magit-diff-options)
    ;; 	    (magit-dont-ignore-whitespace)
    ;; 	  (magit-ignore-whitespace)))

    ;; (defun magit-ignore-whitespace ()
    ;; 	(interactive)
    ;; 	(add-to-list 'magit-diff-options "-w")
    ;; 	(magit-refresh))

    ;; (defun magit-dont-ignore-whitespace ()
    ;; 	(interactive)
    ;; 	(setq magit-diff-options (remove "-w" magit-diff-options))
    ;; 	(magit-refresh))

    ;; use W to cycle through whitespace handling when diffing
    ;; (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

    ;; do not show data loss warning
  ;; (setq magit-last-seen-setup-instructions "1.4.0"))

(provide 'packages-magit)
