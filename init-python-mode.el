;;;
;; init-python-mode.el -- Initialize emacs for python development

;; Mike Barker <mike@thebarkers.com>
;; January 29th 2010

;; Use this to initialize python-mode.el NOT the default python.el that
;; comes with Emacs 23.x.

;; Originaly cribed from Ryan McGuire at http://www.enigmacurry.com
;; See http://www.emacswiki.org/emacs/PythonMode#toc13 for ipython settings

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.\\(py\\|pyw\\)" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'python-mode)

(setq interpreter-mode-alist
      (cons '("python" . python-mode)
	    interpreter-mode-alist)
      python-mode-hook
      '(lambda () (progn
		    (set-variable 'py-indent-offset 4)
		    (set-variable 'py-smart-indentation nil)
		    (set-variable 'indent-tabs-mode nil) 
		    ;;(highlight-beyond-fill-column)
                    ;;(define-key python-mode-map "\C-m" 'newline-and-indent)
		    ;;(pabbrev-mode)
		    ;;(abbrev-mode)
	 )
      )
)

;; Initialize pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call  "pymacs")
(autoload 'pymacs-eval  "pymacs" nil t)
(autoload 'pymacs-exec  "pymacs" nil t)
(autoload 'pymacs-load  "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; Only load ropemacs when needed.
(defun load-ropemacs ()
 "Load pymacs and ropemacs"
 (interactive)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
  ;; Automatically save project python buffers before refactorings
  ;;(setq rope-confirm-saving 'nil)
)

;; ipython for shell
;;(require 'ipython)
;;(setq py-python-command-args '( "--colors" "Linux"))

(provide 'init-python-mode)
