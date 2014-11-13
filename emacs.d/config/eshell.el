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

;;; Git helper functions
(defun mrb:git-p (pwd)
  "Is git installed and is pwd a git project."
  (> (length (and (eshell-search-path "git")
		  (locate-dominating-file pwd ".git"))) 0))

(defun mrb:git-status-cmd (pwd)
  "Run the git status command on the pwd."
  (split-string (shell-command-to-string
		 (concat "cd " pwd  " && git status --porcelain"))))

(defun mrb:git-branch-cmd (pwd)
  "Run the git branch command on the pwd."
  (split-string (shell-command-to-string
		 (concat "cd " pwd " && git branch --no-color --no-colum"))))

(defun mrb:git-branch-name (pwd)
  "Get the branch name for pwd."
  (let ((git-branch (mrb:git-branch-cmd pwd)))
	(if (> (length git-branch) 2)
	    (nth 1 git-branch)
	  ("no branch"))))

;;; Configure the prompt
(defun mrb:prompt-tilde-for-home (pwd)
  "Returns a path with the home directory replaced with a tilde"
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and (>= (length pwd) home-len)
	     (equal home (substring pwd 0 home-len)))
	(concat "~" (substring pwd home-len)) pwd)))

(defun mrb:prompt-git-branch-name (pwd)
  "Return the current git branch as a string,
or the empty string if pwd is not in a git repo,
or the git command is not found."
  (if (mrb:git-p pwd)
      (let ((git-output (mrb:git-branch-name pwd)))
	(when (> (length git-output))
	  (concat "(" git-output ")")))
    (concat "")))

(defun mrb:prompt-root-or-user (pwd)
  "Different prompt chars for root or user."
  (if (= (user-uid) 0) "#" "$"))

(setq eshell-prompt-function
      (lambda nil
	(concat
	 (user-login-name)
	 "@"
	 (system-name)
	 ": "
	 (mrb:prompt-tilde-for-home(eshell/pwd))
	 "\n"
	 (mrb:prompt-git-branch-name(eshell/pwd))
	 (mrb:prompt-root-or-user(eshell/pwd))
	 " ")))

;;; eshell.el ends here.
