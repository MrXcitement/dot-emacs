;;; malabar-mode:

;; Provide support for editing java source and integration with Maven
;; for handling projects.
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
