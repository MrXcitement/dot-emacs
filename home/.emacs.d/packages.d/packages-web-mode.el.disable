;;; packages-web-mode --- Install and configure the `web-mode' package.

;; Mike Barker <mike@thebarkers.com>

;;; Commentary:
;; Configure the web mode major mode.
;; web-mode.el is an autonomous emacs major-mode for editing web templates.
;; HTML documents can embed parts (CSS / JavaScript) and blocks (client / server side).
;; http://web-mode.org/

;;; History:

;;; Code:
(use-package web-mode
  :disabled
  :ensure t

  ;; :mode
  ;; (("\\.html?\\'" . web-mode)
  ;; ("\\.css\\'" . web-mode)
  ;; ("\\.js\\'" . web-mode))

  :bind
  (("C-c C-v" . browse-url-of-buffer)
   ("C-M-;" . comment-dwim))

  :config
  (progn
    (add-hook 'web-mode-hook
      (lambda ()
	;;; set the indention for markup, code and css
	(let ((indent-offset 2))
	  (setq web-mode-markup-indent-offset indent-offset)
	  (setq web-mode-code-indent-offset indent-offset)
	  (setq web-mode-css-indent-offset indent-offset))
	;;(setq web-mode-enable-current-column-highlight t)
	;;(setq web-mode-enable-current-element-highlight t)

	;;; setup auto completion
       	(add-to-list
	 'web-mode-ac-sources-alist
	 '("css" . (ac-source-css-property
		    ac-source-words-in-buffer)))

	(add-to-list
	 'web-mode-ac-sources-alist
	 ;; attribute-value better to be first
	 '("html" . (ac-source-html-attribute-value
		     ac-source-html-tag
		     ac-source-html-attribute)))

	(auto-complete-mode t)

	;;; setup yas snippet behaviour
	(yas-minor-mode-on)
	(yas-activate-extra-mode 'html-mode)))

    (add-hook 'web-mode-before-auto-complete-hooks
      (lambda ()
    	(let ((web-mode-cur-language
    	       (web-mode-language-at-pos)))
    	  (if (string= web-mode-cur-language "javascript")
    	      (yas-activate-extra-mode 'javascript-mode)
    	    (yas-deactivate-extra-mode 'javascript-mode))
    	  (if (string= web-mode-cur-language "css")
    	      (yas-activate-extra-mode 'css-mode)
    	    (yas-deactivate-extra-mode 'css-mode)))))
    ))

(provide 'packages-web-mode)
