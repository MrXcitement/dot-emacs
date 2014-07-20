;; init-packages.el -- Initialize the package manager and the installed packages

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012.11.18
;; * First release.

;; 2012.11.24
;; * Modularize the installation and initialization of packages.
;;   Instead of having a list of packages to be installed and then
;;   each package being initialized. I have seperated the installation
;;   to be done in the same block with the initialization code.

;; 2012.12.20 MRB
;; * Remove redo+, version 1.15 is currently broken.
;; * Add undo-tree, this allows you to *see* how emacs native
;;   undo/redo system works.
;; * Added my-package-refresh-contents defun that will refresh the
;;   package database only if the passed in package name is not in
;;   it allready.

;; 2013.01.08 MRB
;; * Changed keybinding for buffer move to <C-c> <s-{up,down,left,right}>

;; 2013-03-17 MRB
;; * Added git-gutter mode.
;;   https://github.com/syohex/emacs-git-gutter

;; 2013-05-28 MRB
;; * Edited comments

;; 2013-08-20 MRB
;; * removed extra packages.
;; * remove helm, since it does not support emacs 23 and replace with
;;   smex an ido interface to M-x.

;; 2014-02-11 MRB
;; * removed marmalade repository -- not currently using any packages from there.
;; * overhauled the way packages are installed and configured.
;; * packages are added to an install list and each package in this list is
;;   installed at the end of this script.
;; * packages have an after-init hook created that will init them when the
;;   init.el has completed.

;; 2014-02-27 MRB
;; * Added elpy mode
;; * Some code/comment reformatting

;;; Helper functions: init:package(s)-*

;;; Refresh the package database, but only if the package name is not found.
(defun init:package-refresh-contents (package)
  "Refresh the package database if the package name is not found."
  (let ((pkg-desc (assq package package-archive-contents)))
    (unless pkg-desc
      (message "Refreshing the package database")
      (package-refresh-contents))))

;;; Install a single package.
;;; Only install a package that is not allready installed
;;; 2013-05-02 MRB - Ignore errors when installing packages. This
;;; allows a system that does not have access to the various package
;;; archives to continue to work and not stop the initialization of
;;; emacs.
(defun init:package-install (package)
  "Install a single package."
  (unless (package-installed-p package)
    (message "Installing package: %s" package)
    (ignore-errors
      (init:package-refresh-contents package)
      (package-install package))))

;;; Install a list of packages.
(defun init:packages-install (package-list)
  "Install a list of packages."
  (loop for p in package-list do
	(init:package-install p)))

;;; Initialize the Package Manager
(message "init-packages -- Initializing emacs package manager...")

;;; Initialize the package-archives to be used.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Higlight the selected package
(add-hook 'package-menu-mode-hook
	  (lambda() (hl-line-mode 1)))

;;; Add packages to be installed to this empty list.
(setq init:my-packages '())

;;; auto-complete:
(add-to-list 'init:my-packages 'auto-complete )
(add-hook 'after-init-hook
	  (lambda ()
	    (when (require 'auto-complete-config nil )
	      (ac-config-default)
	      (ac-flyspell-workaround)
	      ;; Enable auto-complete in ielm mode
	      (add-hook 'ielm-mode-hook
			(lambda()
			  (setq ac-sources '(ac-source-functions
					     ac-source-variables
					     ac-source-features
					     ac-source-symbols
					     ac-source-words-in-same-mode-buffers))
			  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
			  (auto-complete-mode 1)))
	      )))

;;; buffer-move:
(add-to-list 'init:my-packages 'buffer-move)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-set-key (kbd "C-c <S-up>")     'buf-move-up)
	    (global-set-key (kbd "C-c <S-down>")   'buf-move-down)
	    (global-set-key (kbd "C-c <S-left>")   'buf-move-left)
	    (global-set-key (kbd "C-c <S-right>")  'buf-move-right)))

;;; csharp-mode:
(add-to-list 'init:my-packages 'csharp-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))))

;;; git-gutter:
;;; show git status in the gutter of the file
(add-to-list 'init:my-packages 'git-gutter)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-git-gutter-mode t)
	    (global-set-key (kbd "C-c C-g") 'git-gutter:toggle)
	    (global-set-key (kbd "C-c g =") 'git-gutter:popup-hunk)
	    (global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
	    (global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
	    (global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)))

;;; iy-go-to-char:
;;; provide the ability to quicly go/jump to a character.
(add-to-list 'init:my-packages 'iy-go-to-char)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-set-key (kbd "C-c m") 'iy-go-to-char)
	    (global-set-key (kbd "C-c M") 'iy-go-to-char-backward)))

;;; magit:
;;; Git helper mode
(add-to-list 'init:my-packages 'magit)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'init-magit nil t)))

;;; markdown:
(add-to-list 'init:my-packages 'markdown-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$" .
			     markdown-mode)) auto-mode-alist))))

;;; ntcmd:
(add-to-list 'init:my-packages 'ntcmd)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(bat\\|cmd\\)$" .
			     ntcmd-mode)) auto-mode-alist))))

;;; ** WARNING **
;;; powershell-mode:
;;; this package in melpa is out of date, the latest is on emacswiki,
;;; moved to init-site-lisp.el
;;; ** WARNING **

;;; powershell:
;;; on ms windows, allow an inferior powershell shell
(when (string-equal "windows-nt" system-type)
  (add-to-list 'init:my-packages 'powershell)
  (add-hook 'after-init-hook
	    (lambda ()
	      (require 'powershell nil t))))

;;; semx:
;;; Smex is a M-x enhancement that uses IDO.
(add-to-list 'init:my-packages 'smex)
(add-hook 'after-init-hook
	  (lambda ()
	    (smex-initialize)
	    (global-set-key (kbd "M-x") 'smex)
	    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
	    (global-set-key (kbd "C-c M-x") 'execute-extended-command)))

;;; undo-tree:
;;; Show the undu history as a tree that can be navigated with the arrow keys
(add-to-list 'init:my-packages 'undo-tree)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-undo-tree-mode)))

;;; yasnippet:
;;; snippet template engine
(add-to-list 'init:my-packages 'yasnippet)
(add-hook 'after-init-hook
	  (lambda ()
	    (yas-global-mode)))

;;; elpy:
;;; Emacs Python Development Environment
(add-to-list 'init:my-packages 'elpy)
(add-hook 'after-init-hook
	  (lambda ()
	    (elpy-enable)
	    (elpy-clean-modeline)))

;;; lua-mode:
;;; lua major mode
(add-to-list 'init:my-packages 'lua-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(lua\\)$" .
			     lua-mode)) auto-mode-alist))))

;;; malabar-mode:
;; A better java mode
;; when mvn is available, load this mode
(when (mrb:eshell-command-exist-p "mvn")
  (add-to-list 'init:my-packages 'malabar-mode)
  (add-hook 'after-init-hook
	    (lambda ()
	      (setq semantic-default-submodes
		    '(global-semantic-idle-scheduler-mode
		      global-semanticdb-minor-mode
		      global-semantic-idle-summary-mode
		      global-semantic-mru-bookmark-mode))
	      (semantic-mode 1)
	      (require 'malabar-mode)
	      (setq malabar-groovy-lib-dir "~/lib/malabar/lib")
	      (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
	      )))

;;; Initialize the package manager and installed packages.
(package-initialize)

;;; Install any missing packages
(init:packages-install init:my-packages)

(provide 'init-packages)
