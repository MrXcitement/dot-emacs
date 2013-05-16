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

;;; Modes that have been used in the past, but are not loaded now
;; evernote-mode                ; ** BROKEN ** evernote client
;; w3m				; ** used by evernote mode ** w3m browser
;; org-mode			; org-mode customization
;; haskell			; Haskel mode
;; lolcode                      ; LOLCode, program like a cat
;; python-mode			; Python mode
;; python                       ; Python tools
;; pymacs                       ; Python emacs bridge
;; ipython                      ; Interactive python mode
;; ruby				; Edit ruby files
;; jscomint                     ; Run javascript files
;; mode-compile			; Smart compile configuration
;; ecb				; Emacs Code Browser setup
;; dvc                          ; Distributed version control
;; mercurial			; Mercurial VC support
;; git                          ; GIT mode

;;; helper functions

;; Refresh the package database, but only if the package name is not found.
(defun my-package-refresh-contents (name)
  (let ((pkg-desc (assq name package-archive-contents)))
    (unless pkg-desc
      (message "Refreshing the package database")
      (package-refresh-contents))))

;; Install a single package.
;; Only install a package that is not allready installed
;; 2013-05-02 MRB - Ignore errors when installing packages.
;; This allows a system that does not have access to the various package archives to
;; continue to work and not stop the initialization of emacs.
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

;;; Configure packages/modes

;; buffer-move:
(my-package-install 'buffer-move)
(when (package-installed-p 'buffer-move)
  (global-set-key (kbd "C-c <up>") 
		  (lambda () (interactive) 
		    (call-interactively 'buf-move-up)))
  (global-set-key (kbd "C-c <down>")
		  (lambda () (interactive) 
		    (call-interactively 'buf-move-down)))
  (global-set-key (kbd "C-c <left>") 
		  (lambda () (interactive) 
		    (call-interactively 'buf-move-left)))
  (global-set-key (kbd "C-c <right>")  
		  (lambda () 
		    (interactive) (call-interactively 'buf-move-right))))

;; highlight-80+:
(my-package-install 'highlight-80+)
(when (package-installed-p 'highlight-80+)
  (highlight-80+-mode))

;; iy-go-to-char:
;; Provide the ability to go to a character.
(my-package-install 'iy-go-to-char)
(when (package-installed-p 'iy-go-to-char)
  (global-set-key (kbd "C-c m") 
		  (lambda () 
		    (interactive) (call-interactively 'iy-go-to-char)))
  (global-set-key (kbd "C-c M")
		  (lambda () (interactive) 
		    (call-interactively 'iy-go-to-char-backward))))

;; undo-tree:
(my-package-install 'undo-tree)
(when (package-installed-p 'undo-tree)
  ;;(require 'undo-tree nil t)
  (global-undo-tree-mode))

;; csharp-mode:
(my-package-install 'csharp-mode)
(when (package-installed-p 'csharp-mode)
  (setq auto-mode-alist
	(append '(("\\.cs$" . csharp-mode)) auto-mode-alist)))

;; ntcmd:
(my-package-install 'ntcmd)
(when (package-installed-p 'ntcmd)
  (setq auto-mode-alist
	(append '(("\\.\\(bat\\|cmd\\)$" . ntcmd-mode)) auto-mode-alist)))

;; markdown:
(my-package-install 'markdown-mode)
(when (package-installed-p 'markdown-mode)
  (setq auto-mode-alist
	(append '(("\\.\\(markdown\\|md\\|mdw\\|mdt\\)$" . markdown-mode)) auto-mode-alist)))

;; powershell-mode: allow you to edit powershell files.
(my-package-install 'powershell-mode)
(when (package-installed-p 'powershell-mode)
  (require 'powershell-mode nil t)
  (setq auto-mode-alist
	(append '(("\\.ps1$" . powershell-mode)) auto-mode-alist)))

;; powershell: allow a inferior powershell shell
(when (string-equal "windows-nt" system-type)
  (my-package-install 'powershell)
  (when (package-installed-p 'powershell)
    (require 'powershell nil t)))

;; magit: Git mode
(my-package-install 'magit)
(eval-after-load "magit"
  '(progn
     (require 'init-magit nil t)))

;; auto-complete:
(my-package-install 'auto-complete)
(when (package-installed-p 'auto-complete)
  (require 'auto-complete-config nil t)
  (ac-config-default))

;; yasnippet:
(my-package-install 'yasnippet)
(when (package-installed-p 'yasnippet)
  ;; Makefiles will now include text-mode snippets
  (add-hook 'makefile-mode-hook
	    (lambda()
	      (make-local-variable 'yas-extra-modes)
	      (setq yas-extra-modes 'text-mode)))
  (yas-global-mode t))

;; helm:
(when (>= emacs-major-version 24)
  (my-packages-install '(helm helm-themes))
  (when (package-installed-p 'helm)
    (global-set-key (kbd "C-c h")   'helm-mini)
    (helm-mode 1)))

;; php+-mode:
(my-package-install 'php+-mode)
(when (package-installed-p 'php+-mode)
  (eval-after-load "php+-mode"
    '(progn
       (require 'php+-mode)
       (php+-mode-setup))))

;; git-gutter:
(my-package-install 'git-gutter)
(when (package-installed-p 'git-gutter)
  (global-git-gutter-mode t)
  (global-set-key (kbd "C-c C-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-c g =") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-c g p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-c g n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk))

(provide 'init-packages)

;;; Packages that are not loaded
;; ecb:
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-snap/")
;; (setq stack-trace-on-error t)
;; (require 'ecb)

;; (set-face-foreground (quote ecb-default-highlight-face) "DimGray")
;; (set-face-background (quote ecb-default-highlight-face) "grey60")

;; evernote-mode:
;; (when (require 'evernote-mode)
;;   (setq evernote-username "mrbarker")
;;   (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;;   (global-set-key "\C-ceb" 'evernote-browser)
;;   (global-set-key "\C-cec" 'evernote-create-note)
;;   (global-set-key "\C-ceo" 'evernote-open-note)
;;   (global-set-key "\C-cep" 'evernote-post-region)
;;   (global-set-key "\C-ces" 'evernote-search-notes)
;;   (global-set-key "\C-ceS" 'evernote-do-saved-search)
;;   (global-set-key "\C-cew" 'evernote-write-note))

;; js-comint:
;; (when (require 'js-comint)
;;   ;; Load hs-minor-mode to hide/show blocks
;;   (add-hook 'js-mode-hook
;;	    (lambda ()
;;	      ;; Scan the file for nested code blocks
;;	      (imenu-add-menubar-index)
;;	      ;; Activate the folding mode
;;	      (hs-minor-mode t)))
;;   ;; Use node as our repl
;;   (setq inferior-js-program-command "node")
;;   (setq inferior-js-mode-hook
;;	(lambda ()
;;	  ;; We like nice colors
;;	  (ansi-color-for-comint-mode-on)
;;	  Deal with some prompt nonsense
;;	  (add-to-list 'comint-preoutput-filter-functions
;;		       (lambda (output)
;;			 (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
;;						   (replace-regexp-in-string ".*1G.*3G" ">" output)))))))

;; org-mode:
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-6.34c/lisp") t)
;; (require 'org-install)

;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;; ;; Set to the location of your Org files on your local system
;; (setq org-directory "~/Dropbox/Personal/Org")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))

;; ;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))

;; ;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/MobileOrg")

;; ;; We need to update the checksum file to only transfer updated files.
;; (add-hook 'org-mobile-post-push-hook
;;        (lambda () (shell-command "find ~/Dropbox/MobileOrg -name '*.org' -type f -print | sed 's/^\\.\\///' | xargs md5 > ~/Dropbox/MobileOrg/checksums.dat")))

;; ;; Remember mode integration
;; (org-remember-insinuate)
;; (define-key global-map "\C-cr" 'org-remember)
;; (setq org-remember-templates
;;      '(
;;       ("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" (concat org-directory "/tasks.org") "Tasks")
;;       ("Note" ?n "\n* %^{topic} %T \n%i%?\n" (concat org-directory "/notes.org"))
;;       ("Journal" ?j "\n* %U \n%i%?\n" (concat org-directory "/journal.org"))
;;       ))

;; (setq org-refile-targets (quote (("notes.org" :level . 1)
;;				 ("someday.org" :level . 2)
;;				 ("tasks.org" :maxlevel . 1)
;;				 )))

;; ;; Personal agenda files
;; ;; ===================================================================
;; ;; calendar.org       Birthdays, Aniverseries, etc  <-> Google Calendar
;; ;; journal.org        Daily information             <-> Evernote
;; ;; notes.org          Random notes                  <-> Evernote
;; ;; someday.org        Tasks to be done someday      <-> RTM
;; ;; tasks.org          Projects and tasks            <-> RTM
;; (setq org-agenda-files (list (concat org-directory "/calendar.org")
;;			     (concat org-directory "/journal.org")
;;			     (concat org-directory "/notes.org")
;;			     (concat org-directory "/someday.org")
;;			     (concat org-directory "/tasks.org")
;;			     ))

;; org-feed
;; (setq org-feed-alist
;;       '(("Remember The Milk"
;;          "https://www.rememberthemilk.com/atom/mrbarker/"
;;          "~/Dropbox/Personal/Org/rtm.org" "Remember The Milk Entries"
;;	 :parse-feed org-feed-parse-atom-feed
;;	 :parse-entry org-feed-parse-rtm-entry
;;	 )))

;; (defun org-feed-parse-rtm-entry (entry)
;;   "Parse the `:item-full-text' as a sexp and create new properties."
;;   (let ((xml (car (read-from-string (plist-get entry :item-full-text)))))
;;     ;; Get first <link href='foo'/>.
;;     (setq entry (plist-put entry :link
;;                            (xml-get-attribute
;;                             (car (xml-get-children xml 'link))
;;                             'href)))
;;     ;; Add <title/> as :title.
;;     (setq entry (plist-put entry :title
;;                            (car (xml-node-children
;;                                  (car (xml-get-children xml 'title))))))
;;     (let* ((content (car (xml-get-children xml 'content)))
;;            (type (xml-get-attribute-or-nil content 'type)))
;;       (when content
;;         (cond
;;          ((string= type "text")
;;           ;; We like plain text.
;;           (setq entry (plist-put entry :description (car (xml-node-children content)))))
;;          ((string= type "html")
;;           ;; TODO: convert HTML to Org markup.
;;           (setq entry (plist-put entry :description (car (xml-node-children content)))))
;;          ((string= type "xhtml")
;;           ;; TODO: convert XHTML to Org markup.
;;           (setq entry (plist-put entry :description (prin1-to-string (xml-node-children content)))))
;;          (t
;;           (setq entry (plist-put entry :description (format "Unknown '%s' content." type)))))))
;;     entry))
