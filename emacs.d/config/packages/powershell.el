;;; powershell:
(when (eq system-type 'windows-nt)
  (mrb:package-install 'powershell)
  (add-hook 'after-init-hook
    (lambda()
      (require 'powershell nil t))) t)
