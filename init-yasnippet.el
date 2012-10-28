;;; 
;; init-yasnippet.el
;; Initialize the yasnippet package
;; See: https://github.com/capitaomorte/yasnippet

;; This file is NOT a part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; Febuary 4th, 2010

;;; License

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

;;; Change log:
;; 2012.09.28 MRB
;; * No longer need to add to load-path since it is installed
;;   via package.el.

;; 2012.10.27 MRB
;; * No longer setting the personal directory, was causing problems!?
;; * Add message re initializing yasnippet

(message "Loading init-yasnippet.el...")

(when (require 'yasnippet nil t)
  ;; Develop and keep personal snippets under ~/.emacs.d/mysnippets
  ;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here

