;;;
;; init-package.el
;; Initialize the package manager system
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html

;; This file is NOT part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; Feburary 2, 2010

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

(message "Loading init-package.el...")

;; Initialize the package manager system
(package-initialize)

;; Add third party package archives
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("SC"   . "http://joseito.republika.pl/sunrise-commander/"))

;; A list of packages to install
(defvar init-package-packages '(anything 
				anything-config 
				anything-match-plugin 
				auto-complete 
				buffer-move
				color-theme-sanityinc-tomorrow 
				ecb-snapshot
				groovy-mode
				highlight-80+
				inkpot-theme
				ipython
				multi-term
				ntcmd
				popup
				pymacs
				redo+
				yasnippet)
  "A list of packages to ensure are installed.")

;; Function to install default packages
(defun init-package-install ()
  "Install a default set of packages."
  (interactive)
  (message "Installing default packages...")
  (package-refresh-contents)
  (dolist (package init-package-packages)
    (when (not (package-installed-p package))
      (package-install package)))
  )

(provide 'init-package)
