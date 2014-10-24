;;; iy-go-to-char:
(mrb:package-install 'iy-go-to-char)
(message "package: iy-go-to-char installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: iy-go-to-char initializing...")
    (global-set-key (kbd "C-c m") 'iy-go-to-char)
    (global-set-key (kbd "C-c M") 'iy-go-to-char-backward)) t)
