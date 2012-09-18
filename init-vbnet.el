;;;
;; Load vbnet support
(setq load-path (append '("~/share/emacs/site-lisp/vbnet") load-path))

(autoload 'vbnet-mode "vbnet-mode" "Visual Basic mode." t)
(setq auto-mode-alist 
      (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vbs\\)$" .
		 vbnet-mode)) auto-mode-alist))

(provide 'init-vbnet)