;;; init.el --- My Emacs initialization file

;; Copyright (c) 2012 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 10, 2007

;; This file is NOT part of GNU Emacs

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

;;; 2015.01.29 MRB
;; * Simplify my initialization into one single file.



;;;;;;
;;; Emacs 24 or greater only
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))



;;;;;;
;;; Initialize standard emacs features
(add-to-list
 'load-path (expand-file-name "init.d" user-emacs-directory))

(let ((init-files '(init-customize
		    init-defuns
		    init-environment
		    init-lock-buffers
		    init-save-backup
		    init-keymaps
		    init-ui
		    init-server
		    init-packages)))
  (dolist (init-file init-files)
    (progn
      (message "%S" init-file)
      (require init-file nil t))))


;;;;;;
;;; Report the init load time
(add-hook 'after-init-hook
	  (lambda ()
	    (message "init completed in %.2fms"
		     (* 1000.0
			(float-time
			 (time-subtract after-init-time before-init-time))))))

(provide 'init)
;;; init.el ends here
