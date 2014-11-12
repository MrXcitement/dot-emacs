;;; auto-complete:
(mrb:package-install 'auto-complete)
(message "package: auto-complete installing...")
(add-hook 'after-init-hook
  (lambda()
    (message "package: auto-complete initializing...")
    (require 'auto-complete-config)
    (ac-config-default)
    (auto-complete-mode 1)) t)

(eval-after-load 'auto-complete
  '(progn
     ;; apply workaround when flyspell is enabled
     (add-hook 'flyspell-mode
	       (lambda()
		 (ac-flyspell-workaround)))

    ;; Enable auto-complete in ielm mode
    (add-hook 'ielm-mode-hook
    	      (lambda()
    		(setq ac-sources '(ac-source-functions
    				   ac-source-variables
    				   ac-source-features
    				   ac-source-symbols
    				   ac-source-words-in-same-mode-buffers))
    		(add-to-list 'ac-modes 'inferior-emacs-lisp-mode)))))
