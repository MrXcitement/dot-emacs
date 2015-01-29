;; visual-basic-mode.el
;; http://www.emacswiki.org/emacs/download/visual-basic-mode.el
(mrb:site-lisp-ensure-lib-from-url
 'visual-basic-mode "https://raw.githubusercontent.com/MrXcitement/visual-basic-mode/master/visual-basic-mode.el")

(autoload
  'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)

(setq
 auto-mode-alist
 (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . visual-basic-mode))
	 auto-mode-alist))
