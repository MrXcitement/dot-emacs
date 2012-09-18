;;;
;; Configure ruby support
;; Based on http://infolab.stanford.edu/~manku/dotemacs.html
;; and on http://www.hyperionreactor.net/node/43

;;(setq load-path (append '("~/.emacs.d/site-lisp/ruby") load-path))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process" t)

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode" t)

;; If you have Emacs 19.2x or older, use rubydb2x
(autoload 'rubydb "rubydb3x"
  "Ruby debugger" t)

;; Ruby mode hook setup
(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
	    ;; Indention 2 spaces no tabs
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)

	    ;; Include IMenu
            (imenu-add-to-menubar "IMenu")

            (define-key ruby-mode-map "\C-m" 'newline-and-indent)
            (inf-ruby-keys)

            ;; Electric mode (auto insert ending item)
            ;; (require 'ruby-electric)
            ;; (ruby-electric-mode t)

            ;; create ruby-eval-buffer command
            (defun ruby-eval-buffer () (interactive)
	      "Evaluate the buffer with ruby."
	      (shell-command-on-region (point-min) (point-max) "ruby"))
            (define-key ruby-mode-map "\C-c\C-c" 'ruby-eval-buffer)
            ))

(provide 'init-ruby)