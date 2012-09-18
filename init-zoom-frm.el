;;;
;; Allow the frame text to be zoomed in and out, for thoose about
;; to loose their eyesight, we salute you.
;; This file is NOT a part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; May 21st, 2009

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
;; 2012.09.16 - Put conditional around require, so that if frame-frm is not installed
;;              a message will be printed but not fail.

;; Requires:  
;; * zoom-frm.el
;; * frame-cmd.el
;; * frame-fns.el
;; * misc-fns.el
;; * strings.el
;; Available from: http://www.emacswiki.org/emacs/ElispArea
;; Install using auto-install

;; Only load if on a window system
(if (not(eq window-system nil))
    (progn
      (if (require 'zoom-frm nil t)
	  (progn
	    (global-set-key (kbd "C->") 'zoom-frm-in)
	    (global-set-key (kbd "C-<") 'zoom-frm-out)
	    (global-set-key (kbd "C-.") 'zoom-frm-unzoom))
	(message "WARNING: Unable to load zoom-frm"))))

(provide 'init-zoom-frm)
