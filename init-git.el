;;;
;; load-git.el
;; load and initialize the git libaries
(setq load-path (cons (expand-file-name "~/share/emacs/site-lisp/git") load-path))
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

