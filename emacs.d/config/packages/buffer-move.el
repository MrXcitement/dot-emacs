;;; buffer-move:
(mrb:package-install 'buffer-move)
(eval-after-load 'buffer-move-autoloads
  (progn
    (global-set-key (kbd "C-c <S-up>")     'buf-move-up)
    (global-set-key (kbd "C-c <S-down>")   'buf-move-down)
    (global-set-key (kbd "C-c <S-left>")   'buf-move-left)
    (global-set-key (kbd "C-c <S-right>")  'buf-move-right)))
