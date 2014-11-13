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
(defun mrb:git-p ()
  "Is git installed and the cwd is a git project."
  (> (length (and (eshell-search-path "git")
		  (locate-dominating-file default-directory ".git"))) 0))

(defun mrb:git-status-cmd ()
  "Run the git status command in the cwd."
  (split-string (shell-command-to-string
		 "git status --porcelain")))

(defun mrb:git-branch-cmd ()
  "Run the git branch command in the cwd and return a list of branches."
  (split-string (shell-command-to-string
		 "git branch --no-color --no-colum") "\n"))

(defun mrb:git-branch-name ()
  "Get the current branch name in the cwd."
  (let ((git-branches (mrb:git-branch-cmd)))
    (if (> (length git-branches) 0)
	(dolist (branch git-branches)
	  (when (string-prefix-p "*" branch)
	    (return (substring branch 2 nil))))
      (concat "no branch"))))

;;; Configure the prompt
(defun mrb:prompt-tilde-for-home (dir)
  "Returns a path with the home directory replaced with a tilde"
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and (>= (length dir) home-len)
	     (equal home (substring dir 0 home-len)))
	(concat "~" (substring dir home-len)) dir)))

(defun mrb:prompt-git-branch-name ()
  "Return the current git branch as a string,
or the empty string if cwd is not in a git repo,
or the git command is not found."
  (if (mrb:git-p)
      (let ((git-output (mrb:git-branch-name)))
	(when (> (length git-output) 0)
	  (concat "(" git-output ")")))
    (concat "")))

(defun mrb:prompt-root-or-user ()
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
	 " "
	 (mrb:prompt-git-branch-name)
	 "\n"
	 (mrb:prompt-root-or-user)
	 " ")))

;;; eshell.el ends here.
