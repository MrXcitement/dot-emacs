;;;
;; init-python.el -- Initialize emacs for python development

;; Mike Barker <mike@thebarkers.com>
;; January 29th 2010


;;;
;; ropemacs requires:
;; * rope python module - http://rope.sf.net/
;; * pymacs python module - http://pymacs.progiciels-bpi.ca/pymacs.html
;; * pymacs emacs package 
;; * ropemacs python module - http://rope.sf.net/ropemacs.html

;; Mac OS X Mountain Lion issues: you need to configure system wide
;; environment settings in /etc/launchd.conf file and reboot!

;;;
;; Initializing pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")


;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;; Only load ropemacs when needed.
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq rope-confirm-saving 'nil))

(global-set-key "\C-xpl" 'load-ropemacs)

;; ipython for shell
;;(require 'ipython)
;;(setq py-python-command-args '( "--colors" "Linux"))

(provide 'init-python)
