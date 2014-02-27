;; init-eshell.el -- Initialize the emacs shell

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012.11.18
;; * First release.

;; 2014-02-27 MRB
;; * Added initialization message
;; * Some code reformatted
;; * Add init: prefix to functions

;;; Eshell initialization
(message "init-eshell -- Initializing the eshell mode...")

;; Needed for colors to have an effect
(setq eshell-highlight-prompt nil)

;; Needed to tweek for completion to work
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

;; History settings
(setq eshell-history-size 1024)
(load "em-hist") ;; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t))
(if (boundp 'ehsell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun init:pwd-repl-home (pwd)
  "Returns a path with the home directory replaced with a tilde"
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and (>= (length pwd) home-len)
	     (equal home (substring pwd 0 home-len)))
	(concat "~" (substring pwd home-len)) pwd)))


(defun init:curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (if (and (eshell-search-path "git")
	     (locate-dominating-file pwd ".git"))
      (progn
	(let ((git-output (shell-command-to-string
			   (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
	  (concat "("
		  (if (> (length git-output) 0)
		      (substring git-output 0 -1)
		    "(no branch)")
		  ")")))
    (concat "")))

;; Put a new line after the path and before the prompt
(setq eshell-prompt-function
      (lambda nil
	(concat
	 (user-login-name)
	 "@"
	 (system-name)
	 ": "
	 (init:pwd-repl-home(eshell/pwd))
	 " "
	 (init:curr-dir-git-branch-string(eshell/pwd))
	 "\n"
	 (if (= (user-uid) 0) "# " "$ "))))

(provide 'init-eshell)
