;; init-submodules -- Initialize the submodule packages

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker

;; Change log:
;; 2012.11.18
;; * First release.
;; 2012.11.19
;; * Disabled nXhtml mode... it is almost an editor unto itself and is
;;   more heavy than I would like.

;; initialize packages that have been added as a submodule. These
;; packages are not currently in any of the package repositories. As
;; soon as they are they should be removed from here and added as a
;; package.

;; To work with the submodules use the following git commands.

;; Add a Submodule:
;; git submodule add <path to repository>

;; Update a Submodule:
;; git submodule foreach git checkout master
;; git submodule foreach git pull

;; When cloning the repo
;; git clone <source> <target>
;; git submodule update --init

;; Delete a Submodule:
;; 1. Delete the relevant section from the .gitmodules file.
;; 2. Delete the relevant section from .git/config.
;; 3. Run git rm --cached path_to_submodule (no trailing slash).
;; 4. Commit and delete the now untracked submodule files.


;; vbnet-mode:
(let ((vbnet-mode-path (concat init--emacs-root "/submodules/vbnet-mode")))
      (when (file-exists-p vbnet-mode-path)
	(add-to-list 'load-path vbnet-mode-path)
	(autoload 'vbnet-mode "vbnet-mode" "Visual Basic mode." t)
	(setq auto-mode-alist
	      (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vbs\\)$" .
			 vbnet-mode)) auto-mode-alist))))

(provide 'init-submodules)
