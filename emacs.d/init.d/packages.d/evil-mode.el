;;; evil-mode --- install and configure the evil-mode plugin

;; Mike Barker <mike@thebarkers.com>
;; June 27th, 2015

;; History:
;; 2016-01-28 MRB
;; * Added evil leader
;; * Added powerline-evil

(use-package powerline-evil
  :ensure t
  :config
  (powerline-evil-center-color-theme))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "e" 'counsel-find-file
    "b" 'switch-to-buffer
    "k" 'kill-buffer))

(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode 1)))
