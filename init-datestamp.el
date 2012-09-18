;;;
;; Date and time functions
;; This file is NOT a part of GNU Emacs

;; Mike Barker
;; September 15, 2012

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

;;;
;;This command prompts the user for a date format
(defun insert-date (format)
    "Wrapper around format-time-string."
    (interactive "MFormat: ")
    (insert (format-time-string format)))

;;;
;; current date and time.
(defun insert-date-current-date-time()
  "Insert the current date and time at point."
  (interactive "*")
  (insert (format-time-string "%Y.%m.%d %H:%M:%S"  (current-time))))

;;;
;; current date.
(defun insert-date-current-date()
  "Insert the current date and time at point."
  (interactive "*")
  (insert (format-time-string "%Y.%m.%d"  (current-time))))

;;;
;; This command will insert the standard date format
(defun insert-date-standard ()
    "Inserts standard date time string."
    (interactive)
    (insert (format-time-string "%c")))

(provide 'init-datestamp)
