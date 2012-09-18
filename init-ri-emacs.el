;;;
;; Load ri emacs

(setq ri-ruby-script 
      (expand-file-name "~/.emacs.d/elisp/ri-emacs/ri-emacs.rb"))
(autoload 'ri "~/.emacs.d/elisp/ri-emacs/ri-ruby.el" nil t)
;;
;;  You may want to bind the ri command to a key.
;;  For example to bind it to F1 in ruby-mode:
;;  Method/class completion is also available.
;;
;;   (add-hook 'ruby-mode-hook (lambda ()
;;                               (local-set-key 'f1 'ri)
;;                               (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;;                               (local-set-key 'f4 'ri-ruby-show-args)
;;                               ))