;; init-javascript.el
;; Initialize the javascript major mode and configure.

;; Mike Barker <mike@thebarkers.com>
;; October 21, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.10.21
;; * First release.


;; Load hs-minor-mode to hide/show blocks
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

(when (require 'js-comint)
;; Use node as our repl
  (setq inferior-js-program-command "node")
  (setq inferior-js-mode-hook
	(lambda ()
	  ;; We like nice colors
	  (ansi-color-for-comint-mode-on)
	  Deal with some prompt nonsense
	  (add-to-list 'comint-preoutput-filter-functions
	  	       (lambda (output)
	  		 (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
	  					   (replace-regexp-in-string ".*1G.*3G" ">" output))))
	  ))
  )

(provide 'init-javascript)
