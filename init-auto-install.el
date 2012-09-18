;;;
;; Initialize the auto install package
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

;; 2012.09.15 - Added MIT License from the OSI (http://opensource.org/licenses/MIT)

;; See http://www.emacswiki.org/emacs/AutoInstall

(message "Loading init-auto-install.el...")

;; Append the auto-install repository
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install.d") t)

(require 'auto-install)
(setq auto-install-directory (expand-file-name "~/.emacs.d/auto-install.d/"))

(provide 'init-auto-install)
