;;; auto-complete:

;; This package provides auto complete in a drop down window similar
;; to intelisense in visual studio.

(use-package auto-complete
  :ensure t

  :init
  (require 'auto-complete-config)

  :config
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    (setq ac-auto-start 2)
    (setq ac-ignore-case nil)

    ;; fixup problems with flyspell mode
    (add-hook 'flyspell-mode
    	      (lambda()
    		(ac-flyspell-workaround)))

    ;; set the correct sources for ielm mode
    (add-hook 'ielm-mode-hook
    	      (lambda()
    		(setq ac-sources '(ac-source-functions
    				   ac-source-variables
    				   ac-source-features
    				   ac-source-symbols
    				   ac-source-words-in-same-mode-buffers))
    		(add-to-list 'ac-modes 'inferior-emacs-lisp-mode)))

    ;; turn off auto-complete menu when in elpy mode
    (add-hook 'elpy-mode-hook
	      (lambda()
		(message "turning off auto-complete menu...")
		(make-local-variable 'ac-auto-start)
		(setq ac-auto-start nil)))
    ))
