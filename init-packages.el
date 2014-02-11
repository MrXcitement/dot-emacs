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

;;; Helper functions: my-package(s)-*

;;; Refresh the package database, but only if the package name is not found.
(defun init-packages--package-refresh-contents (package)
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
(defun init-packages--package-install (package)
  "Install a single package."
  (unless (package-installed-p package)
    (message "Installing package: %s" package)
    (ignore-errors
      (init-packages--package-refresh-contents package)
      (package-install package))))

;;; Install a list of packages.
(defun init-packages--packages-install (package-list)
  "Install a list of packages."
  (loop for p in package-list do
	(init-packages--package-install p)))

;;; Initialize the Package Manager
(message "init-package -- Initializing emacs package manager...")

;;;
;;; Install and configure packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;; 			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; Hook the package menu mode
(add-hook 'package-menu-mode-hook
	  (lambda() (hl-line-mode 1)))

;;; Add packages to be installed to this list.
(setq init-packages--packages '())

;;; auto-complete:
(add-to-list 'init-packages--packages 'popup)
(add-to-list 'init-packages--packages 'auto-complete)
(add-hook 'after-init-hook
	  (lambda ()
	    (when (require 'auto-complete-config nil t)
	      (ac-config-default))))

;;; buffer-move:
(add-to-list 'init-packages--packages 'buffer-move)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-set-key (kbd "C-c <S-up>")     'buf-move-up)
	    (global-set-key (kbd "C-c <S-down>")   'buf-move-down)
	    (global-set-key (kbd "C-c <S-left>")   'buf-move-left)
	    (global-set-key (kbd "C-c <S-right>")  'buf-move-right)))

;;; csharp-mode:
(add-to-list 'init-packages--packages 'csharp-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))))

;;; git-gutter:
;;; show git status in the gutter of the file
(add-to-list 'init-packages--packages 'git-gutter)
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
(add-to-list 'init-packages--packages 'iy-go-to-char)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-set-key (kbd "C-c m") 'iy-go-to-char)
	    (global-set-key (kbd "C-c M") 'iy-go-to-char-backward)))

;;; magit:
;;; Git helper mode
(add-to-list 'init-packages--packages 'magit)
(add-hook 'after-init-hook
	  (lambda ()
	    (require 'init-magit nil t)))

;;; markdown:
(add-to-list 'init-packages--packages 'markdown-mode)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$" .
			     markdown-mode)) auto-mode-alist))))

;;; ntcmd:
(add-to-list 'init-packages--packages 'ntcmd)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq auto-mode-alist
		  (append '(("\\.\\(bat\\|cmd\\)$" .
			     ntcmd-mode)) auto-mode-alist))))

;;; powershell-mode:
;;; ** WARNING **
;;; this package in melpa is out of date, the latest is on emacswiki,
;;; moved to init-site-lisp.el
;;; ** WARNING **
;;; allow you to edit powershell files.
;;; (my-package-install 'powershell-mode)
;;; (when (package-installed-p 'powershell-mode)
;;;   (require 'powershell-mode nil t)
;;;   (setq auto-mode-alist
;;; 	(append '(("\\.ps1$" . powershell-mode)) auto-mode-alist)))

;;; powershell:
;;; on ms windows, allow an inferior powershell shell
(when (string-equal "windows-nt" system-type)
  (add-to-list 'init-packages--packages 'powershell)
  (add-hook 'after-init-hook
	    (lambda ()
	      (require 'powershell nil t))))

;;; semx:
;;; Smex is a M-x enhancement that uses IDO.
(add-to-list 'init-packages--packages 'smex)
(add-hook 'after-init-hook
	  (lambda ()
	    (smex-initialize)
	    (global-set-key (kbd "M-x") 'smex)
	    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
	    (global-set-key (kbd "C-c M-x") 'execute-extended-command)))

;;; undo-tree:
;;; Show the undu history as a tree that can be navigated with the arrow keys
(add-to-list 'init-packages--packages 'undo-tree)
(add-hook 'after-init-hook
	  (lambda ()
	    (global-undo-tree-mode)))

;;; yasnippet:
;;; snippet template engine
(add-to-list 'init-packages--packages 'yasnippet)
(add-hook 'after-init-hook
	  (lambda ()
	    (yas-global-mode t)
	    ;; Hook modes here to allow them to have a specific set of snippets available
	    ;; makefiles will now include text-mode snippets
	    (add-hook 'makefile-mode-hook
		      (lambda()
			(make-local-variable 'yas-extra-modes)
			(setq yas-extra-modes 'text-mode)))
	    ))

;;; Initialize the package manager and installed packages.
(package-initialize)

;;; Install any missing packages
(init-packages--packages-install init-packages--packages)

(provide 'init-packages)
