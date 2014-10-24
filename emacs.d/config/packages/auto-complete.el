;;; auto-complete:
(mrb:package-install 'auto-complete)
(eval-after-load 'auto-complete-autoloads
  (progn
    (message "*** auto-complete-autoloads has been loaded ***")
    (require 'auto-complete-config)
    (ac-config-default)

    ;; apply workaround when flyspell is enabled
    (add-hook 'flyspell-mode
	      (lambda()
		(ac-flyspell-workaround)))

    ;; Enable auto-complete in ielm mode
    ;; (add-hook 'ielm-mode-hook
    ;; 	      (lambda()
    ;; 		(setq ac-sources '(ac-source-functions
    ;; 				   ac-source-variables
    ;; 				   ac-source-features
    ;; 				   ac-source-symbols
    ;; 				   ac-source-words-in-same-mode-buffers))
    ;; 		(add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
    ;; 		(auto-complete-mode 1)))
    ))
