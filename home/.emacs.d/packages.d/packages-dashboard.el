;;; packages-dashboard.el --- Install and configure the emacs dashboard package

;; Mike Barker <mike@thebarkers.com>
;; November 29th, 2023

;;; Commentary:
;; Provide a 'dasboard' window at start that provides custom information

;;; History:
;; 2023.11.29
;; * First release.

;;; Code:
(use-package dashboard
 :ensure t
 :bind (:map dashboard-mode-map
        ("g" . 'dashboard-refresh-buffer))
 :config
 (setq dashboard-items '((bookmarks . 5)
                         (recents   . 5)))
 (dashboard-setup-startup-hook))

(provide 'packages-dashboard)
