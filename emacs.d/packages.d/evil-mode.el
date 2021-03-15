;;; evil-mode --- install and configure the evil-mode plugin

;; Mike Barker <mike@thebarkers.com>
;; June 27th, 2015

;; History:
;; 2016-01-28 MRB
;; * Added evil leader
;; * Added powerline-evil

;; 2021-03-15 MRB
;; Copied evil setup from here:
;; - https://ryan.himmelwright.net/post/emacs-update-evil-usepackage/

;; Evil Mode
(use-package evil
  :ensure t
  :config

  (evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "s s" 'swiper
      "d x w" 'delete-trailing-whitespace
      "e" 'counsel-find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme)))
