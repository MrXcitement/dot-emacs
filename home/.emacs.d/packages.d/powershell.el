;;; powershell

;; Provide an powershell shell buffer when on windows.
(when (eq system-type 'windows-nt)
  (use-package powershell
    :ensure t
    :config
    (require 'powershell nil t)))
