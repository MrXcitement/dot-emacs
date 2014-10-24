;;; malabar-mode:
(when (mrb:eshell-command-exist-p "mvn")
  (mrb:package-install 'malabar-mode)
  (message "package: malabar installing...")
  (add-hook 'after-init-hook
    (lambda()
      (message "package: malabar initializing...")
      (setq semantic-default-submodes
	    '(global-semantic-idle-scheduler-mode
	      global-semanticdb-minor-mode
	      global-semantic-idle-summary-mode
	      global-semantic-mru-bookmark-mode))
      (semantic-mode 1)
      (require 'malabar-mode)
      (setq malabar-groovy-lib-dir "~/lib/malabar/lib")
      (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode)))) t)
