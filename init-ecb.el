;;; 
;; init-ecb.el
;; Initialize the ecb package
;; http://ecb.sourceforge.net/

;; This file is NOT a part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; May 21st, 2009

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
;; 2012.09.19
;; * First release.

;; ECB requires the CEDET system
(require 'init-cedet)

;; Initialize the ECB system
(add-to-list 'load-path "~/.emacs.d/site-lisp/ecb-snap/")
(setq stack-trace-on-error t)
(require 'ecb)

(set-face-foreground (quote ecb-default-highlight-face) "DimGray")
(set-face-background (quote ecb-default-highlight-face) "grey60")

(provide 'init-ecb)
;;; init-ecb.el ends here
