;;; 
;; init-evernote-mode.el
;; Initialize the evernote package.
;; This package allows you to use emacs as a frontend to evernote.

;; This file is NOT a part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; March 06, 2011

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

;;; Requires:
;; * evernote-mode.el
;; * ruby scripts provided in tarball to interact with evernote service.
;; * optionaly the w3m program to format enml notes.
;; http://code.google.com/p/emacs-evernote-mode/

;; Installed evernote-mode.el via package.el

;;; Change log:
;; 2011/03/06
;; * First released.
;; 2011/12/13
;; * Added evernote-username variable
;; 2012.09.15 
;; * Added MIT License from the OSI (http://opensource.org/licenses/MIT)

(when (require 'evernote-mode)
  (setq evernote-username "mrbarker")
  (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
  (global-set-key "\C-ceb" 'evernote-browser)
  (global-set-key "\C-cec" 'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note))

(provide 'init-evernote-mode)

;;; init-evernote-mode.el ends here
