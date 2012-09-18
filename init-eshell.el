;;;
;; Initialize the eshell package
;; see: http://www.emacswiki.org/emacs/CategoryEshell
;; This file is NOT part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; February 19, 2010

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
;;
;; 2010/02/19
;; * First released.
;; * Add some color to the prompt
;; 2012.09.15 
;; * Added MIT License from the OSI (http://opensource.org/licenses/MIT)

(message "Initialize the eshell package...")

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;; Put a new line after the path and before the prompt
  (setq eshell-prompt-function (lambda nil
        (concat
	 (with-face (user-login-name) :foreground "blue")
	 (with-face "@" :foreground "blue")
	 (with-face (system-name) :foreground "blue")
	 " "
	 (with-face (eshell/pwd) :foreground "green")
         "\n"
	 (if (= (user-uid) 0) "# " "$ "))))

;; Needed for the above colors to have an effect
(setq eshell-highlight-prompt nil)

;; Needed to tweek for completion to work
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

(provide 'init-eshell)

;;; init-eshell.el ends here
