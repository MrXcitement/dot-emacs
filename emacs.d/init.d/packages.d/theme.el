;; theme.el --- Install and configure themes

;; Author: Mike Barker <mike@pooh.thebarkers.lan>
;; Created: November  5, 2015

;; History:
;; 2015.11.05
;; * First release.

(mrb:package-install 'zenburn-theme)

(use-package solarized-theme
  :if window-system
  :defines solarized-theme
  :ensure t
  :init
  ;(load-theme 'solarized-light)
  )

;; (use-package deeper-blue-theme
;;   :if (not window-system)
;;   :init
;;   (load-theme 'deeper-blue))
