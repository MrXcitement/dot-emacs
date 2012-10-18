;;;
;; init-auto-install.el
;; Initialize the auto install package and provide a function to
;; install a default list of packages.

;; Note: If a package can be installed via package.el, it should be
;; used instead. see init-package.el

;; See http://www.emacswiki.org/emacs/AutoInstall

;; This file is NOT part of GNU Emacs

;; Mike Barker <mike@thebarkers.com
;; June 16th, 2009

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
;; * Added MIT License from the OSI (http://opensource.org/licenses/MIT)

;; 2012.09.19
;; * Only require the auto-install package if it exists.
;; * Add interactive function to install and update the auto-install packages

(message "Loading init-auto-install.el...")

(setq init-auto-install-path (expand-file-name "~/.emacs.d/auto-install"))
(setq init-auto-install-file (concat init-auto-install-path "/auto-install.el"))
(setq init-auto-install-url "http://www.emacswiki.org/emacs/download/auto-install.el")
(setq auto-install-directory init-auto-install-path)

;; If auto-install.el file exists
(if (file-exists-p init-auto-install-file)
    (progn
      ;; Append the auto-install repository
      (add-to-list 'load-path init-auto-install-path)
      (when (require 'auto-install)
	(setq auto-install-directory init-auto-install-path))
      )
  (progn
    (message "WARNING: The auto-install package is not installed.")
    (message " To install auto-install type: M-x init-auto-install-install")
    )
)

;; The list of default emacswiki packages
(defvar init-auto-install-packages '("frame-fns.el"
				     "frame-cmds.el"
				     "misc-fns.el"
				     "protbuf.el"
				     "strings.el"
				     "visual-basic-mode.el"
				     "zoom-frm.el")
  "A list of default emacswiki packages to install")

;; Install default emacswiki packages
(defun init-auto-install-install ()
  "Install default emacswiki packages."
  (interactive)
  
  ;; make the auto-install directory
  (unless (file-exists-p init-auto-install-path)
    (mkdir init-auto-install-path))

  ;; download the auto-install.el file into the auto-install.d directory
  (url-copy-file init-auto-install-url init-auto-install-file t t)
  (add-to-list 'load-path init-auto-install-path)
  
  ;; if the auto-install package is installed
  (when (require 'auto-install)
    ;; Downloading emacswiki package list
    (message "init-auto-install...downloading emacswiki package list...")
    (auto-install-update-emacswiki-package-name)
    (sit-for 5)
    (message "init-auto-install...done downloading.")

    ;; Install default emacswiki packages
    (dolist (package init-auto-install-packages) 
      (auto-install-from-emacswiki package))
    )
  )
  
(provide 'init-auto-install)

