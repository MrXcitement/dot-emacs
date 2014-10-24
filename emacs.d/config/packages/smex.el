;;; semx:
(mrb:package-install 'smex)
(message "package: smex installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: smex initializing...")
    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    (global-set-key (kbd "C-c M-x") 'execute-extended-command)) t)
