;;; init.el --- My Emacs initialization file

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

;; 2012.11.01 MRB
;; * wrap require cl in eval-at-compile.
;;   suppress byte compile warnings regarding cl.
;;   define init--emacs-root with the emacs config dir.
;; * handle loading packages from submodules.

;; 2012.11.18 MRB
;; * Added nXhtml submodule
;; * Moved sections back into seperate init files.

;; 2012.11.19 MRB
;; * Added configuration of hideshow (hs-minor-mode) to allow any text
;;   to be hidden and shown.
;; * Added rules to allow hs-minor-mode handle nxml files

;; 2012.12.19 MRB
;; * No longer track the load time for init.el.
;;   If you are intersted in the init time, M-x emacs-init-time.

;; 2013.01.08 MRB
;; * Set windmove-default-keybindings so that super-arrow key will move to
;;   window.

;; 2013.01.23 MRB
;; * Removed enable cua-mode in customize.el
;;   now just set: (cua-selection-mode 1)

;; 2013-05-28 MRB
;; * Added whitespace configuration from init-ui.el file.

;; 2013-08-23 MRB
;; * Misc re-factoring var names, etc.

;; 2013-09-25 MRB
;; * Turn on recent file mode (recentf-mode t)
;; * Clean up some of the comments.

;;;
;; Load the cl package and disable byte compile warnings
(eval-when-compile (require 'cl nil t))
(setq byte-compile-warnings '(cl-functions))

;; Where is the emacs initialization directory
(defvar init-emacs-root (expand-file-name "~/.emacs.d"))

;; Add personal directories to the START of load-path
(add-to-list 'load-path (concat init-emacs-root "/lisp"))

;; Aquamacs has it's own custom.el and some of the default settings in
;; custom.el will cause aquamacs to have problems. Also if Aquamacs puts
;; it's customize settings in the same file, Emacs may have problems
;; when it loads.

;; Unless the current emacs is aquamaces,
;; configure and load custom-file used for customize settings.
(unless (featurep 'aquamacs)
  (setq custom-file (concat init-emacs-root "/custom.el"))
  (load custom-file 'noerror))

;;; Initiaze the system/os environement variables
(require 'init-environment nil t)

;;; Initialize keyboard
(require 'init-keymaps nil t)

;;; Initialize the user interface
(require 'init-ui nil t)

;;; Initialize internal major and minor modes
(require 'init-eshell nil t)
(require 'init-hideshow nil t)
(require 'init-ido nil t)
;;(require 'init-cedet nil t)
(recentf-mode t) ;; Turn on recent file mode

;;; Initialize the spelling sub-system.
(require 'init-spelling nil t)

;;; Initialize buffers to protect and where to put auto-save and backup files.
;;(require 'init-protbufs nil t)
(require 'init-save-backup nil t)

;;; Initialize the server
(require 'init-server nil t)

;;; Hook major and minor modes

;; Remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight the current line when in dired mode.
(add-hook 'dired-mode-hook
	  (lambda()
	    (hl-line-mode 1)))

;;; Initialize 3rd party packages
(require 'init-site-lisp nil t)
(require 'init-packages nil t)

;;; Operating Specific Initialization
(cond
 ;; Darwin (Mac OS X) gui custimzation
 ((eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  )
 ;; Linux gui customization
 ((eq system-type 'gnu/linux)
  )
 ;; Windows customizations
 ((eq system-type 'windows-nt)))

(provide 'init)
;;; init.el ends here
