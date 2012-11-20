;; init-eshell.el -- Initialize the emacs shell 

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Eshell initialization
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;; Put a new line after the path and before the prompt
(setq eshell-prompt-function
      (lambda nil
	(concat
	 (with-face (user-login-name) :foreground "blue")
	 (with-face "@" :foreground "blue")
	 (with-face (system-name) :foreground "blue")
	 " "
	 (with-face (eshell/pwd) :foreground "green")
	 "\n"
	 (if (= (user-uid) 0) "# " "$ "))))

;; Needed for the above colors to have an effect
(setq eshell-highlight-prompt nil)

;; Needed to tweek for completion to work
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

(provide 'init-eshell)
