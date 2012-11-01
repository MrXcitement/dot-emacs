;;;
;; init.el
;; My emacs initialization file

;; This file is NOT part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; October 10, 2007

;; Copyright (c) 2012 Mike Barker

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; 2012.09.15
;; - Added MIT License from the OSI (http://opensource.org/licenses/MIT)

;; 2012.09.16
;; * Implement el-get to replace auto-install and package.el

;; 2012.09.19
;; * Removed el-get and not using at this time.
;;   el-get is not multi platform friendly, it uses symlinks to packages
;;   and this causes Dropbox does not handle them correctly. Also they will
;;   not work in Windows.
;; * init-auto-install handles emacswiki package installs
;; * init-elpa handles installing pakages using package.el

;; 2012.10.31 MRB
;; * moved all settings into this file.
;; * using package system with the melpa repository.

;; Save the current time for testing how long the init.el file took to load
(defvar *init--load-start* (current-time))

;;;
;; Load the cl package
(require 'cl nil t)

;;;
;; Initialize the environment


;; If not loading in aquamacs.
;; Aquamacs has it's own custom.el and some of the settings in custom.el
;; will cause aquamacs to have problems.
(unless (featurep 'aquamacs)
  ;; Configure and load custom-file used for customize settings.
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file 'noerror))

;; Add personal directories to the START of load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; if the user name is not set, set the user full name
(when (eq user-full-name "")
    (setq user-full-name "Mike Barker"))

;; if the email is not set, set it
(when (eq user-mail-address "")
    (setq user-mail-address "mike@thebarkers.com"))

;; Determine the system we are on
(cond
 ((string-equal "darwin" system-type)
  ;; configure mac os x configuration
  (let ((mypaths '("/Users/mike/bin"
		  "/Users/Shared/bin"
		  "/Library/Frameworks/Python.framework/Versions/Current/bin"
		  "/opt/local/bin"
		  "/usr/local/git/bin"
		  "/usr/local/bin"
		  "/usr/X11/bin"
		  "/usr/bin"
		  "/bin")))
    (setenv "PATH" (mapconcat 'identity mypaths ":"))
    (setq exec-path mypaths))
  )
 ((string-equal "gnu/linux" system-type)
  ;; configure linux environment
  )
 ((string-equal "windows-nt" system-type )
  ;; configure windows environment
  ))

;;;
;; Initialize the package manager and repositories
(package-initialize)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

;;;
;; Initialize default set of external packages
(setq my-packages 
      '(frame-fns frame-cmds zoom-frm
	redo+
	buffer-move
	highlight-80+
	iy-go-to-char
	csharp-mode
	ntcmd
;;	visual-basic-mode
	auto-complete
	yasnippet))

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
;; magit
;; anything                       ; Find anything you want
;; anything-config
;; anything-match-plugin

;;;
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

;;;
;; zoom-frm: Only define keys when running as a gui
(when (window-system)
  (global-set-key (kbd "C->") 'zoom-frm-in)
  (global-set-key (kbd "C-<") 'zoom-frm-out)
  (global-set-key (kbd "C-.") 'zoom-frm-unzoom))

;;;
;; redo+:
;; If you don't want to redo a previous undo, add
;; (setq undo-no-redo t)
(global-set-key (kbd "C-?") 'redo)   ; [Ctrl+Shift+/]
(global-set-key (kbd "C-x r") 'redo) ; [Ctrl+x r]

;;;
;; buffer-move:
(global-set-key (kbd "C-c <up>")     'buf-move-up)
(global-set-key (kbd "C-c <down>")   'buf-move-down)
(global-set-key (kbd "C-c <left>")   'buf-move-left)
(global-set-key (kbd "C-c <right>")  'buf-move-right)

;;;
;; highlight-80+:
(highlight-80+-mode t)

;;;
;; iy-go-to-char:
(global-set-key (kbd "C-c m") 'iy-go-to-char)

;;;
;; csharp-mode:
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;;
;; ntcmd:
(autoload 'ntcmd-mode "ntcmd" "Windows batch file mode." t)
(setq auto-mode-alist
      (append '(("\\.\\(bat\\|cmd\\)$" .
		 ntcmd-mode)) auto-mode-alist))

;;;
;; visual-basic-mode:
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist
      (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vbs\\)$" .
		 visual-basic-mode)) auto-mode-alist))

;;;
;; auto-complete:
(when (require 'auto-complete-config nil t)
  (ac-config-default))

;;;
;; yasnippet:
(yas-global-mode t)

;;;
;; Compilation output, next/previous error. (Alt-Page Up/Alt-Page Down)
(global-set-key (kbd "<M-prior>") 'previous-error)
(global-set-key (kbd "<M-next>")  'next-error)

;; System specific key mappings
(cond
 ;; Darwin (Mac OS X) key mappings
 ((string-equal "darwin" system-type)
  (global-set-key [kp-delete] 'delete-char) ; Make fn-del delete forward
  )
 ((string-equal "gnu/linux" system-type)
  ;; Linux key mappings
  )
 ((string-equal "windows-nt" system-type )
  ;; Windows key mappings
  ))

;;;
;; User interface settings 
(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(tool-bar-mode -1)

;; Configure window session
(when (window-system)
  (load-theme 'tango t) ; Set the default color theme
  ;; System specific ui settings
  (cond
   ((eq system-type 'darwin)
    ;; Darwin (Mac OS X) gui custimzation
    (set-face-font 'default "Droid Sans Mono Slashed 14")
    )
   ((eq system-type 'gnu/linux)
    ;; Linux gui customization)
    )
   ((eq system-type 'windows-nt)
    ;; Windows customizations
    (set-face-font 'default "Lucida Console 10")
    )))

;; Configure terminal session
(unless (window-system)
  (menu-bar-mode -1))

;;;
;; Eshell initialization
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;; Put a new line after the path and before the prompt
(setq eshell-prompt-function
      (lambda nil
	(concat
	 (with-face (user-login-name) :foreground "blue")
	 (with-face "@" :foreground "blue")
	 (with-face (system-name) :foreground "blue")
	 " "
	 (with-face (eshell/pwd) :foreground "green")
	 "\n"
	 (if (= (user-uid) 0) "# " "$ "))))

;; Needed for the above colors to have an effect
(setq eshell-highlight-prompt nil)

;; Needed to tweek for completion to work
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

;;;
;; Show whitespace in buffer
(setq-default show-trailing-whitespace t)
(setq indicate-empty-lines t)
;; Remove trailing whitespace when saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;
;; Protect buffers
(save-excursion
  (set-buffer "*scratch*")
  (emacs-lock-mode 'kill)
  (set-buffer "*Messages*")
  (emacs-lock-mode 'kill))

;;;
;; Autosave and backup support
(setq user-temporary-file-directory (expand-file-name "~/tmp/emacs/"))
(make-directory user-temporary-file-directory t)

;; Backup files
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

;; Auto save files
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory "auto-saves-"))
(setq auto-save-file-name-transforms
      `(("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,user-temporary-file-directory t)))

;;;
;; Interactively do things
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Set M-x to use ido mode
(defun ido-execute-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp)))))
(global-set-key (kbd "M-x") 'ido-execute-command)

;;;
;; Start a server for client processes, do not start if one is allredy running
;; Determine the system we are on
(cond
 ((eq system-type 'darwin)
  ;; Darwin (Mac OS X) custimzations
  )
 ((eq system-type 'gnu/linux)
  ;; Gnu/linux customizations
  )
 ((eq system-type 'windows-nt)
  ;; Windows customizations
  (setq server-auth-dir (getenv "TMP"))
  ))

;;;
;; Start a server for client processes, do not start if one is allredy running
(load "server")
(unless (server-running-p)
  (server-start))

;;;
;; How long did it take to initialize emacs.
(message "Emacs initialization (init.el) loaded in %ds"
	 (destructuring-bind (hi lo ms) (current-time)
	   (- (+ hi lo) (+ (first *init--load-start*) (second *init--load-start*)))))

(provide 'init)
;;; init.el ends here
