;;; 
;; init-env-darwin.el
;; Initialize the path and exec-path when running on a darwin (MacOS) system.
;; This file is NOT a part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; September 18, 2012

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
;; 2012.09.18
;; * First release.

(message "Loading init-env-darwin...")
(let ((mypaths '("/Users/mike/bin"
		 "/Users/mike/.rvm/bin"
		 "/Users/Shared/bin"
		 "/Library/Frameworks/Python.framework/Versions/Current/bin"
		 "/opt/local/bin"
		 "/usr/local/git/bin"
		 "/usr/local/bin"
		 "/usr/X11/bin"
		 "/usr/bin"
		"/bin")))

(setenv "PATH" (mapconcat 'identity mypaths ":"))
(setq exec-path mypaths))

(provide 'init-env-darwin)
;;; init-env-darwin.el ends here
