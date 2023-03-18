;; packages-theme.el --- Install and configure themes

;; Author: Mike Barker <mike@pooh.thebarkers.lan>
;; Created: November  5, 2015

;;; Commentary:

;;; History:
;; 2023.03.18 MRB
;; * Install vs-dark and vs-light theme
;; 2015.11.05
;; * First release.

;;; Code:
(use-package vs-dark-theme
  :if window-system
  :ensure t)

(use-package vs-light-theme
  :if window-system
  :ensure t)

(use-package deeper-blue-theme
  :disabled
  :if (not window-system)
  :init
  (load-theme 'deeper-blue))

(provide 'packages-theme)
