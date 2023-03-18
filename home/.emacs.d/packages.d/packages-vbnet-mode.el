;;; packages-vbnet-mode.el

;; Mike Barker <mike@thebarkers.com>

;;; Commentary:
;; http://www.emacswiki.org/emacs/download/vbnet-mode.el

;;; History:
;; 2015.03.13
;; * Now use auto-install to download and setup elisp library.
;; 2015-06-03
;; * Moved from auto-install.d to packages.d
;; 2015-06-04
;; * Switched from visual-basic-mode.el to vbnet-mode.el
;; 2021-03-15 MRB
;; I have currently disabled auto-install, will look into a modern replacement, possibly
;; straight.el would work?

;;; Code:
;; Only download the library if it does not allready exist. Updates will be done manualy
;; (unless (mrb:auto-install-library-exists-p "vbnet-mode.el")
;;   (mrb:auto-install-from-url
;;    "vbnet-mode.el"
;;    "https://raw.githubusercontent.com/MrXcitement/vbnet-mode/master"))

;; (use-package vbnet-mode
;;   :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$")

(provide 'packages-vbnet-mode)
