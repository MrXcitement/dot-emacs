;;; elpy

;; Provides a python programming toolset.
(when (executable-find "python")
  (use-package elpy
    :ensure
    :config
    (elpy-enable)))
