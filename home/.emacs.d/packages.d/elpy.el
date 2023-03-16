;;; elpy

;; Provides a python programming toolset.
(when (executable-find "python3")
  (use-package elpy
    :ensure
    :config
    (setq elpy-rpc-python-command "python3")
    (elpy-enable))
)
