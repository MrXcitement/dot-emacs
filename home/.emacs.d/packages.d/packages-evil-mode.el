;;; packages-evil --- Install and configure the `evil' package.

;; Mike Barker <mike@thebarkers.com>
;; June 27th, 2015

;;; Commentary:
;; Install and configure evil and associated packages.

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; * rename personal functions from mrb:funcname to my/funcname
;; 2021-03-15 MRB
;; Copied evil setup from here:
;; - https://ryan.himmelwright.net/post/emacs-update-evil-usepackage/
;; 2016-01-28 MRB
;; * Added evil leader
;; * Added powerline-evil

;;; Code:
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

  ;; (use-package evil-leader
  ;;   :ensure t
  ;;   :config
  ;;   (global-evil-leader-mode t)
  ;;   (evil-leader/set-leader "<SPC>")
  ;;   (evil-leader/set-key
  ;;     "s s" 'swiper
  ;;     "d x w" 'delete-trailing-whitespace
  ;;     "e" 'counsel-find-file
  ;;     "b" 'switch-to-buffer
  ;;     "k" 'kill-buffer))

  (use-package evil-surround
    :after evil
    :ensure t
    :config (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :after evil
    :ensure t)

  (use-package powerline-evil
    :after evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme)))

(provide 'packages-evil-mode)
