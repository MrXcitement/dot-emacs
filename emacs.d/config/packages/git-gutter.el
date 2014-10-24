;;; git-gutter:
(mrb:package-install 'git-gutter)
(message "package: git-gutter installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: git-gutter initializing...")
    (global-git-gutter-mode t)
    (global-set-key (kbd "C-c C-g") 'git-gutter:toggle)
    (global-set-key (kbd "C-c g =") 'git-gutter:popup-hunk)
    (global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
    (global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
    (global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)) t)
