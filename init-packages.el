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


;;; Helper functions: my-package(s)-*

;; Refresh the package database, but only if the package name is not found.
(defun my-package-refresh-contents (name)
  (let ((pkg-desc (assq name package-archive-contents)))
    (unless pkg-desc
      (message "Refreshing the package database")
      (package-refresh-contents))))

;; Install a single package.
;; Only install a package that is not allready installed
;; 2013-05-02 MRB - Ignore errors when installing packages. This
;; allows a system that does not have access to the various package
;; archives to continue to work and not stop the initialization of
;; emacs.
(defun my-package-install (my-package)
  (unless (package-installed-p my-package)
    (message "Installing package: %s" my-package)
    (ignore-errors
      (my-package-refresh-contents my-package)
      (package-install my-package))))

;; Install a list of packages.
(defun my-packages-install (my-package-list)
  (loop for p in my-package-list do
	(my-package-install p)))

;;; Initialize the emacs package manager
(defun my-packages-initialize-emacs ()
  (message "Initializing emacs package manager...")
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")))
  ;; Hook the package menu mode
  (add-hook 'package-menu-mode-hook
	    (lambda() (hl-line-mode 1))))

;;; Initialize the Package Manager
;; I try to use the package manager and third party repositories for
;; most of the additional packages that I use in my initialization.
(my-packages-initialize-emacs)


;;; Install and configure packages

;;; auto-complete:
(my-packages-install '(popup auto-complete))
(when (package-installed-p 'auto-complete)
  (when (require 'auto-complete-config nil t)
    (ac-config-default)))


;;; buffer-move:
(my-packages-install '(buffer-move))
(when (package-installed-p 'buffer-move)
  (global-set-key (kbd "C-c <S-up>")     'buf-move-up)
  (global-set-key (kbd "C-c <S-down>")   'buf-move-down)
  (global-set-key (kbd "C-c <S-left>")   'buf-move-left)
  (global-set-key (kbd "C-c <S-right>")  'buf-move-right))

;;; csharp-mode:
(my-packages-install '(csharp-mode))
(when (package-installed-p 'csharp-mode)
  (setq auto-mode-alist
	(append '(("\\.cs$" . csharp-mode)) auto-mode-alist)))

;;; git-gutter:
;; show git status in the gutter of the file
(my-package-install 'git-gutter)
(when (package-installed-p 'git-gutter)
  (global-git-gutter-mode t)
  (global-set-key (kbd "C-c C-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-c g =") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk))

;;; highlight-80+:
(my-packages-install '(highlight-80+))
(when (package-installed-p 'highlight-80+)
  (highlight-80+-mode))

;;; iy-go-to-char:
;; provide the ability to quicly go/jump to a character.
(my-package-install 'iy-go-to-char)
(when (package-installed-p 'iy-go-to-char)
  (global-set-key (kbd "C-c m") 'iy-go-to-char)
  (global-set-key (kbd "C-c M") 'iy-go-to-char-backward))

;;; magit:
;; Git helper mode
(my-packages-install '(magit))
(eval-after-load 'magit '(require 'init-magit))

;;; markdown:
(my-packages-install '(markdown-mode))
(when (package-installed-p 'markdown-mode)
  (setq auto-mode-alist
	(append '(("\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$" .
		   markdown-mode)) auto-mode-alist)))

;;; ntcmd:
(my-package-install 'ntcmd)
(when (package-installed-p 'ntcmd)
  (setq auto-mode-alist
	(append '(("\\.\\(bat\\|cmd\\)$" . ntcmd-mode)) auto-mode-alist)))

;;; php+-mode:
(my-packages-install '(php+-mode))
(when (package-installed-p 'php+-mode)
  (require 'php+-mode)
  (php+-mode-setup))

;;; powershell-mode:
;; ** WARNING **
;; this package in melpa is out of date, the latest is on emacswiki,
;; moved to init-site-lisp.el
;; ** WARNING **
;; allow you to edit powershell files.
;; (my-package-install 'powershell-mode)
;; (when (package-installed-p 'powershell-mode)
;;   (require 'powershell-mode nil t)
;;   (setq auto-mode-alist
;; 	(append '(("\\.ps1$" . powershell-mode)) auto-mode-alist)))

;;; powershell:
;; allow an inferior powershell shell
(my-packages-install '(powershell))
(when (and (package-installed-p 'powershell)
	   (string-equal "windows-nt" system-type))
  (require 'powershell nil t))

;;; semx:
;; Smex is a M-x enhancement that uses IDO.
(my-packages-install '(smex))
(when (package-installed-p 'smex)
  (smex-initialize)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  (global-set-key (kbd "M-x") 'smex)
  ;;(global-set-key (kbd "M-x") 'smex-major-mode-commands)
  )

;;; undo-tree:
(my-packages-install '(undo-tree))
(when (package-installed-p 'undo-tree)
  ;;(require 'undo-tree nil t)
  (global-undo-tree-mode))

;;; yasnippet:
;; snippet template engine
(my-package-install 'yasnippet)
(when (package-installed-p 'yasnippet)
  ;; Hook modes here to allow them to have a specific set of snippets available
  ;; makefiles will now include text-mode snippets
  (add-hook 'makefile-mode-hook
	    (lambda()
	      (make-local-variable 'yas-extra-modes)
	      (setq yas-extra-modes 'text-mode)))
  (yas-global-mode t))

(provide 'init-packages)
