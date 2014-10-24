;;; iy-go-to-char:
(mrb:package-install 'iy-go-to-char)
(eval-after-load 'iy-go-to-char-autoloads
  (progn
    (global-set-key (kbd "C-c m") 'iy-go-to-char)
    (global-set-key (kbd "C-c M") 'iy-go-to-char-backward)))
