;;; packages-swiper.el --- Install and configure the `swipper' package
;;; Commentary:
;; This file loads swiper and related packages.
;; The package swiper includes ivy-mode. 'Ivy is a generic completion
;; method for Emacs, similar to icomplete-mode. It aims to be more
;; efficient, more simple, and more pleasant to use than the
;; alternatives. It's also highly customizable and very small.'
;;; History:
;;; Code:
(unless (executable-find "ag")
  (message "%s" "executable: ag not found!, counsel-ag will not work."))

(use-package flx
  :disabled
  :ensure t)

(use-package counsel
  :disabled
  :ensure t)

(use-package swiper
  :disabled
  :ensure t
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate)
   ("C-c u" . counsel-unicode-char)
   ("C-c M-x" . execute-extended-command))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy))))

(provide 'packages-swiper)
