;; vbnet-mode.el
;; http://www.emacswiki.org/emacs/download/vbnet-mode.el

;; 2015.03.13
;; * Now use auto-install to download and setup elisp library.

;; 2015-06-03
;; * Moved from auto-install.d to packages.d

;; 2015-06-04
;; * Switched from visual-basic-mode.el to vbnet-mode.el

;; Only download the library if it does not allready exist. Updates will be done manualy
(unless (mrb:auto-install-library-exists-p "vbnet-mode.el")
  (mrb:auto-install-from-url
   "vbnet-mode.el"
   "https://raw.githubusercontent.com/MrXcitement/vbnet-mode/master"))

(use-package vbnet-mode
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$")
