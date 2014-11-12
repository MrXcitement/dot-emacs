;;; iy-go-to-char:
(mrb:package-install 'iy-go-to-char)
(add-hook 'after-init-hook
  (lambda()
    (global-set-key (kbd "C-c m") 'iy-go-to-char)
    (global-set-key (kbd "C-c M") 'iy-go-to-char-backward)) t)
