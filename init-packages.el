;; init-packages.el -- Initialize the package manager and the installed packages

;; Mike Barker <mike@thebarkers.com>
;; November 18, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.18
;; * First release.

;;; Initialize the Package Manager
;; I try to use the package manager and third party repositories for
;; most of the additional packages that I use in my initialization.

(package-initialize)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

;; a default set of external packages
(setq my-packages 
      '(
	;; ** Miscellaneous Modes **
	frame-fns frame-cmds zoom-frm
	redo+
	buffer-move
	highlight-80+
	iy-go-to-char
	;; ** Language Modes **
	csharp-mode
	ntcmd
	markdown-mode
	;; ** Source Code Management Modes **
	magit
	;; ** Completion Modes **
	auto-complete
	yasnippet
	helm
	helm-themes))

;;; Modes that have been used in the past, but are not loaded now
;; evernote-mode                ; ** BROKEN ** evernote client
;; w3m 		                ; ** used by evernote mode ** w3m browser
;; org-mode    	                ; org-mode customization
;; haskell           	        ; Haskel mode
;; lolcode                      ; LOLCode, program like a cat
;; python-mode 	                ; Python mode
;; python                       ; Python tools
;; pymacs                       ; Python emacs bridge
;; ipython                      ; Interactive python mode
;; ruby	                        ; Edit ruby files
;; jscomint                     ; Run javascript files
;; mode-compile 	        ; Smart compile configuration
;; ecb 		                ; Emacs Code Browser setup
;; dvc                          ; Distributed version control
;; mercurial 	                ; Mercurial VC support
;; git                          ; GIT mode

;; Install any of my packages not allready installed
(defun my-packages-installed-p ()
  (every 'package-installed-p my-packages))

(unless (my-packages-installed-p)
  (message "%s" "Refreshing the package database")
  (package-refresh-contents)
  (message "%s" "...done.")
  (loop for p in my-packages
	unless (package-installed-p p)
	do (package-install p)))

;;; Configure packages/modes

;; zoom-frm:
;; Only define keys when running as a gui
(when (package-installed-p 'zoom-frm)
  (when (window-system)
    (global-set-key (kbd "C->") 'zoom-frm-in)
    (global-set-key (kbd "C-<") 'zoom-frm-out)
    (global-set-key (kbd "C-.") 'zoom-frm-unzoom)))

;; redo+:
;; If you don't want to redo a previous undo, add
;; (setq undo-no-redo t)
(when (package-installed-p 'redo+)
  (require 'redo+ nil t)
  (global-set-key (kbd "C-?")   'redo)  ; [Ctrl+Shift+/]
  (global-set-key (kbd "C-x r") 'redo)) ; [Ctrl+x r]

;; buffer-move:
(when (package-installed-p 'buffer-move)
  (global-set-key (kbd "C-c <up>")     'buf-move-up)
  (global-set-key (kbd "C-c <down>")   'buf-move-down)
  (global-set-key (kbd "C-c <left>")   'buf-move-left)
  (global-set-key (kbd "C-c <right>")  'buf-move-right))

;; highlight-80+:
(when (package-installed-p 'highlight-80+)
  (highlight-80+-mode t))

;; iy-go-to-char:
(when (package-installed-p 'iy-go-to-char)
  (global-set-key (kbd "C-c m") 'iy-go-to-char))

;; csharp-mode:
(when (package-installed-p 'csharp-mode)
  (setq auto-mode-alist
	(append '(("\\.cs$" . csharp-mode)) auto-mode-alist)))

;; ntcmd:
(when (package-installed-p 'ntcmd)
  (setq auto-mode-alist
	(append '(("\\.\\(bat\\|cmd\\)$" .
		   ntcmd-mode)) auto-mode-alist)))

;; markdown:
(when (package-installed-p 'markdown-mode)
  (setq auto-mode-alist
	(append '(("\\.\\(text\\|markdown\\|md\\|mdw\\|mdt\\)$" .
		   markdown-mode)) auto-mode-alist)))

;; magit:
(when (package-installed-p 'magit)
  ;; configure magit here.
  )

;; auto-complete:
(when (package-installed-p 'auto-complete)
  (when (require 'auto-complete-config nil t)
    (ac-config-default)))

;; yasnippet:
(when (package-installed-p 'yasnippet)
  (yas-global-mode t))


;; helm:
(when (package-installed-p 'helm)
  (global-set-key (kbd "C-c h")   'helm-mini)
  (helm-mode 1))


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
;; 	    (lambda ()
;; 	      ;; Scan the file for nested code blocks
;; 	      (imenu-add-menubar-index)
;; 	      ;; Activate the folding mode
;; 	      (hs-minor-mode t)))
;;   ;; Use node as our repl
;;   (setq inferior-js-program-command "node")
;;   (setq inferior-js-mode-hook
;; 	(lambda ()
;; 	  ;; We like nice colors
;; 	  (ansi-color-for-comint-mode-on)
;; 	  Deal with some prompt nonsense
;; 	  (add-to-list 'comint-preoutput-filter-functions
;; 	  	       (lambda (output)
;; 	  		 (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
;; 	  					   (replace-regexp-in-string ".*1G.*3G" ">" output)))))))

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
;; 				 ("someday.org" :level . 2)
;; 				 ("tasks.org" :maxlevel . 1)
;; 				 )))

;; ;; Personal agenda files
;; ;; ===================================================================
;; ;; calendar.org       Birthdays, Aniverseries, etc  <-> Google Calendar
;; ;; journal.org        Daily information             <-> Evernote
;; ;; notes.org          Random notes                  <-> Evernote
;; ;; someday.org        Tasks to be done someday      <-> RTM
;; ;; tasks.org          Projects and tasks            <-> RTM
;; (setq org-agenda-files (list (concat org-directory "/calendar.org")
;; 			     (concat org-directory "/journal.org")
;; 			     (concat org-directory "/notes.org")
;; 			     (concat org-directory "/someday.org")
;; 			     (concat org-directory "/tasks.org")
;; 			     ))

;; org-feed
;; (setq org-feed-alist
;;       '(("Remember The Milk"
;;          "https://www.rememberthemilk.com/atom/mrbarker/"
;;          "~/Dropbox/Personal/Org/rtm.org" "Remember The Milk Entries"
;; 	 :parse-feed org-feed-parse-atom-feed
;; 	 :parse-entry org-feed-parse-rtm-entry
;; 	 )))

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

