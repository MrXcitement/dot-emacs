;; init-ido.el -- Initialize the Interactively DO mode 

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Interactively do things
(ido-mode t)
(setq ido-enable-flex-matching t) ; enable fuzzy matching

;; Set M-x to use ido mode
(defun ido-execute-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp)))))
(global-set-key (kbd "M-x") 'ido-execute-command)

(provide 'init-ido)


