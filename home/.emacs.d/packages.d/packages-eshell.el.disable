;;; packages.eshell.el --- Configure the emacs shell

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; Create an eshell prompt that includes git status

;;; History:
;; 2023.03.22
;; * rename personal functions from `my/funcname' to `my-funcname'
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; * rename personal functions from mrb:funcname to my/funcname
;; 2014.11.12
;; * removed loading message

;;; Code:

;;; Git helper functions
(defun my-git-p ()
  "Is git installed and the cwd is a git project."
  (> (length (and (eshell-search-path "git")
		  (locate-dominating-file default-directory ".git"))) 0))

(defun my-git-status-cmd ()
  "Run the git status command in the cwd."
  (split-string (shell-command-to-string
		 "git status --porcelain")))

(defun my-git-branch-cmd ()
  "Run the git branch command in the cwd and return a list of branches."
  (split-string (shell-command-to-string
		 "git branch --no-color") "\n"))

(defun my-git-branch-name ()
  "Get the current branch name in the cwd."
  (let ((git-branches (my-git-branch-cmd)))
    (if (> (length git-branches) 0)
	(dolist (branch git-branches)
	  (when (string-prefix-p "*" branch)
	    (return (substring branch 2 nil))))
      (concat "no branch"))))


;;; Configure the prompt
(defun my-prompt-tilde-for-home (dir)
  "Returns a path with the home directory replaced with a tilde"
  (let* ((home (expand-file-name (getenv "HOME")))
	 (home-len (length home)))
    (if (and (>= (length dir) home-len)
	     (equal home (substring dir 0 home-len)))
	(concat "~" (substring dir home-len)) dir)))

(defun my-prompt-git-branch-name ()
  "Return the current git branch as a string,
or the empty string if cwd is not in a git repo,
or the git command is not found."
  (if (my-git-p)
      (let ((git-output (my-git-branch-name)))
	(when (> (length git-output) 0)
	  (concat "(" git-output ")")))
    (concat "")))

(defun my-prompt-root-or-user ()
  "Different prompt chars for root or user."
  (if (= (user-uid) 0) "#" "$"))

(defun my-prompt-function ()
  (concat
   (user-login-name)
   "@"
   (car (split-string (system-name) "\\."))
   ": "
   (my-prompt-tilde-for-home(eshell/pwd))
   " "
   (my-prompt-git-branch-name)
   "\n"
   (my-prompt-root-or-user)
   " "))

;;; Configure the eshell package
(use-package eshell
  :init
  (require 'esh-mode)
  :config
  (progn
    ;; Needed for colors to have an effect
    (setq eshell-highlight-prompt nil)
    ;; Needed to tweek for completion to work
    (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
    ;; History settings
    ;; make sure the history vars are defined
    (setq eshell-history-size 1024)
    (if (boundp 'eshell-save-history-on-exit)
	(setq eshell-save-history-on-exit t))
    (if (boundp 'ehsell-ask-to-save-history)
	(setq eshell-ask-to-save-history 'always))
    ;; Set the prompt function
    (setq eshell-prompt-function 'my-prompt-function)))

(provide 'packages-eshell)
