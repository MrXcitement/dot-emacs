;;; powershell:
(when (eq system-type 'windows-nt)
  (mrb:package-install 'powershell)
  (message "package: powershell installing...")
  (add-hook 'after-init-hook
    (lambda()
      (message "package: powershell initializing...")
      (require 'powershell nil t))) t)
