;;; malabar-mode:

;; Provide support for editing java source and integration with Maven
;; for handling projects.

(when (mrb:eshell-command-exist-p "mvn")
  (package-install 'malabar-mode))

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
    (setq malabar-groovy-lib-dir "~/lib/malabar/lib")))
