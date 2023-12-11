;;; packages-malabar-mode --- Install and configure the `malabar-mode' package.

;;; Commentary:
;; Provide support for editing java source and integration with Maven
;; for handling projects.

;;; History:

;;; Code:
(when (executable-find "mvn")
  (use-package malabar-mode
    :mode "\\.java\\'"
    :config
    (progn
      (setq semantic-default-submodes
	    '(global-semantic-idle-scheduler-mode
	      global-semanticdb-minor-mode
	      global-semantic-idle-summary-mode
	      global-semantic-mru-bookmark-mode))
      (semantic-mode 1)
      (setq malabar-groovy-lib-dir "~/lib/malabar/lib"))))

(provide 'packages-malabar-mode)
