;;; eshell.el --- Initialize the emacs shell

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message

;;; Needed for colors to have an effect
(setq eshell-highlight-prompt nil)

;;; Needed to tweek for completion to work
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

;;; History settings
;; make sure the history vars are defined
(load "em-hist")
(setq eshell-history-size 1024)
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t))
(if (boundp 'ehsell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always))

(defun mrb:pwd-tilde-for-home (pwd)
  "Returns a path with the home directory replaced with a tilde"
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and (>= (length pwd) home-len)
	     (equal home (substring pwd 0 home-len)))
	(concat "~" (substring pwd home-len)) pwd)))


(defun mrb:pwd-current-git-branch (pwd)
  "When not running on windows.
Return the current git branch as a string,
or the empty string if pwd is not in a git repo,
or the git command is not found."
  (interactive)
  (when (not (eq system-type 'windows-nt))
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
      (concat ""))))

;;; Configure the prompt
;; Put a new line after the path and before the prompt
(setq eshell-prompt-function
      (lambda nil
	(concat
	 (user-login-name)
	 "@"
	 (system-name)
	 ": "
	 (mrb:pwd-tilde-for-home(eshell/pwd))
	 "\n"
	 (mrb:pwd-current-git-branch(eshell/pwd))
	 (if (= (user-uid) 0) "#" "$")
	 " ")))

;;; eshell.el ends here.
