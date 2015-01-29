;;; init.el --- My Emacs initialization file

;; Copyright (c) 2012 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 10, 2007

;; This file is NOT part of GNU Emacs

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

;; 2014-02-27 MRB
;; * Moved in line code into separate packages in the lisp dir.
;;   Created init-os and init-hooks modules to handle setting os
;;   specific settings and create global hooks.
;; * Removed hard coding the users emacs directory, now using
;;   user-emacs-directory variable.
;; * Error if Emacs earlier than version 24
;; * Major cleanup of code.


;;; 2014.10.22 MRB
;; * Emacs Modular Configuration

;;; 2014.11.12 MRB
;; * Simplified modular configuration setup

;;; Load the cl package and disable byte compile warnings
(eval-when-compile (require 'cl nil t))
(setq byte-compile-warnings '(cl-functions))

;;; Emacs 24 or greater only
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;; Emacs Modular Configuration
;; In an effort to make my emacs configuration more modular I am using
;; the information found at the Emacs Wiki's Dot Emacs Modular article,
;; http://www.emacswiki.org/emacs-en/DotEmacsModular
;; I can now add initialization files configuration directory and they
;; will be loaded at startup.
;; TODO: Add the capability to byte compile updated files and load the
;; byte-compiled file
(defun mrb:load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (mrb:load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(mrb:load-directory "~/.emacs.d/init.d")

;;; Load time
(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (* 1000.0
			(float-time
			 (time-subtract after-init-time before-init-time))))))

(provide 'init)
;;; init.el ends here
