;;; init.el --- My emacs initialization file

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

;;;
;; Load the cl package and disable byte compile warnings
(eval-when-compile (require 'cl nil t))	
(setq byte-compile-warnings '(cl-functions))

;;; Initialize the environment
(defvar init--emacs-root (expand-file-name "~/.emacs.d"))

;; Add personal directories to the START of load-path
(add-to-list 'load-path init--emacs-root)

;; Aquamacs has it's own custom.el and some of the default settings in
;; custom.el will cause aquamacs to have problems.
(unless (featurep 'aquamacs)
  ;; Configure and load custom-file used for customize settings.
  (setq custom-file (concat init--emacs-root "/custom.el"))
  (load custom-file 'noerror))

(require 'init-environment nil t)

;;; Initialize 3rd party packages
(require 'init-site-lisp nil t)
;;(require 'init-submodules nil t)
(require 'init-packages nil t)

;;; Initialize keyboard
(require 'init-keymaps nil t)

;;; Initialize the user interface
(require 'init-ui nil t)
;; turn on move to window support (<super-{up,down,left,right}>)
(windmove-default-keybindings 'super)

;;; Initialize internal major and minor modes
(require 'init-eshell nil t)
(require 'init-hideshow nil t)
(when (< emacs-major-version 24)
  (require 'init-ido nil t))	       ; use ido on emacs less than 24
;;(require 'init-cedet nil t)
(cua-selection-mode 1)			; allow cua rectangle selection

;;; Hook major and minor modes
(add-hook 'dired-mode-hook
	  (lambda()
	    (hl-line-mode 1)))

;;; Spelling configuration
(setq ispell-program-name "hunspell")

;;; Whitespace configuration
;; (setq-default show-trailing-whitespace t)
;; (setq indicate-empty-lines t)
;; Remove trailing whitespace when saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Intialize buffers to protect and where to put autosave and backup files.
;;(require 'init-protbufs nil t)
(require 'init-save-backup nil t)

;;; Initialize the server
(require 'init-server nil t)

(provide 'init)
;;; init.el ends here
