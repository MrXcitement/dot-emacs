;; visual-basic-mode.el
;; http://www.emacswiki.org/emacs/download/visual-basic-mode.el
(mrb:site-lisp-ensure-lib-from-url
 'visual-basic-mode
 "https://raw.githubusercontent.com/MrXcitement/visual-basic-mode/master/visual-basic-mode.el")

(use-package visual-basic-mode
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$")
