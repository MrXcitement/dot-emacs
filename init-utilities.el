;;;
;; Filename: init-utilities.el
;; Description: Initialize utility functions.
;;              rotate-windows --
;; Author: Mike Barker <mike@thebarkers.com>
;; Maintainer: Mike Barker <mike@thebarkers.com>
;; Copyright (C) 2010, Mike Barker, all rights reserved.
;; Created: 2010-04-24 09:23:07
;; Version: 0.1
;; Last-Updated: 2010-04-24 09:23:07
;;           By: Mike Barker

;;; This file is NOT part of GNU Emacs


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

;; Rotate the windows
(defun rotate-windows ()
  "Rotate your windows" (interactive)
  (cond ((not (> (count-windows) 1))
	 (message "You can't rotate a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while  (< i numWindows)
	   (let* ((w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))

		  (s1 (window-start w1))
		  (s2 (window-start w2)))

	     (set-window-buffer w1  b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))

;;;
;; Show whitespace in buffer
(setq-default show-trailing-whitespace t)
(setq default-indicate-empty-lines t)

;;;
;; Remove trailing whitespace when saving
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-utilities)

;;; init-utilities.el ends here
