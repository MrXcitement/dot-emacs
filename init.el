;;;
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
;; - Implement el-get to replace auto-install and package.el

;; Add personal .emacs.d and site-lisp dir to START of load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/themes"))

;; Save the current time for testing how long the init.el fie took to load
(defvar *emacs-load-start* (current-time))

;; If not loading in aquamacs...
(when (not (featurep 'aquamacs))
  ;; Configure and load custom-file used for customize settings.
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file 'noerror)
)

;;;
;; if the user name is not set, set the user full name
(if (= (length user-full-name) 0)
    (setq user-full-name "Mike Barker"))

;;;
;; Load the cl package
(require 'cl nil t)

;;;
;; Initialize the environment (PATH and exec-path)
(require 'init-env nil t)

;;;
;; EmacsWiki auto-install utilities
;; Required by: zoom-frm
;; http://www.emacswiki.org/emacs/AutoInstall
(require 'init-auto-install nil t)

;;;
;; Emacs 24 and ELPA package management system
;; http://tromey.com/elpa/
(require 'init-elpa nil t)

;;; 
;; Misc personalization
;; (require 'init-color-theme nil t)	; Color theme support (no longer needed)
(require 'init-keys nil t)		; Key mappings
(require 'init-ui nil t)		; Init the user interface (fonts, etc)
(require 'init-redo+ nil t)		; Undo/Redo functionality
(require 'init-protbuf nil t)		; Protect buffers
(require 'init-auto-save nil t)		; Autosave and backup support
(require 'init-datestamp nil t)		; Date/time stamp support
(require 'init-zoom-frm nil t)		; Zoom/Unzoom frame
(require 'init-eshell nil t)		; eshell customization
;;(require 'init-evernote-mode nil t)	; ** BROKEN ** evernote client
;;(require 'init-w3m nil t)		; ** used by evernote mode ** w3m browser
(require 'init-buffer-move nil t)	; Move buffer to window
(require 'init-utilities nil t)		; misc utility functions
;; (require 'init-org-mode nil t)		; org-mode customization

;; Programming languages
;; (require 'init-csharp nil t )	; C# configuration
;; (require 'init-vbnet nil t )		; VB mode
;; (require 'init-ruby nil t )		; Ruby mode
;; (require 'init-python-mode nil t )	; Python mode
;; (require 'init-haskell nil t )	; Haskel mode
;; (require 'init-lolcode nil t )       ; LOLCode, program like a cat
(require 'init-python nil t)            ; Python tools
(require 'init-ntcmd-mode nil t)	; NT(DOS) command files.

;; Programming tools
(require 'init-mode-compile nil t )	; Smart compile configuration
(require 'init-ecb nil t )		; Emacs Code Browser setup

;; Source code management
;;(require 'init-dvc nil t )              ; Distributed version control
;;(require 'init-mercurial nil t )	; Mercurial VC support
;;(require 'init-git nil t )            ; GIT mode

;; Completion utils
(require 'init-yasnippet nil t)         ; Template support
(require 'init-auto-complete nil t)     ; http://cx4a.org/software/auto-complete/
(require 'init-ido nil t) 		; IDO comand completion system
(require 'init-anything nil t)		; Find anything you want

;;;
;; Start a server for client processes, do not start if one is allredy running
(require 'init-server nil t )

;;;
;; How long did it take to initialize emacs.
(message "Emacs initialization (init.el) loaded in %ds"
	 (destructuring-bind (hi lo ms) (current-time)
	   (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(provide 'init)
;;; init.el ends here
