;; visual-basic-mode.el
;; http://www.emacswiki.org/emacs/download/visual-basic-mode.el

;; 2015.03.13
;; * Now use auto-install to download and setup elisp library.

;; 2015-06-03
;; * Moved from auto-install.d to packages.d

;; Only download the library if it does not allready exist. Updates will be done manualy
(unless (mrb:auto-install-library-exists-p "visual-basic-mode.el")
  (mrb:auto-install-from-url
   "visual-basic-mode.el"
   "https://raw.githubusercontent.com/emacsmirror/visual-basic-mode/master"))

(use-package visual-basic-mode
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$")
