;;;
;; Set general purpose key mappings and system specific mappings
;; This file is NOT part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; June 24, 2012

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

(message "Loading init-keys.el...")

;; Show recent files buffer
(global-set-key (kbd "C-x M-f") 'recentf-open-files)

;; Compilation output, next/previous error. (Page Up/Page Down)
(global-set-key (kbd "<M-prior>") 'previous-error)
(global-set-key (kbd "<M-next>")  'next-error)

;; Determine the system we are on
(cond
 ;; Darwin (Mac OS X) custimzations
 ((string-equal "darwin" system-type)
  (global-set-key [kp-delete] 'delete-char) ; Make fn-del delete forward
  )

 ;; Gnu/linux customizations
 ((string-equal "gnu/linux" system-type)
  )

 ;; Windows customizations
 ((string-equal "windows-nt" system-type )
  )
)

(provide 'init-keys)
