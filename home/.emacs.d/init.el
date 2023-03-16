;;; init.el --- My Emacs initialization file

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

;;;;;;
;;; History

;; 2021-03-15 MRB
;; Reorganize init files
;; - Moved packages from ~/.emacs.d/init.d to ~/.emacs.d/packages.d
;; - Now require any .el file in the init.d directory
;; - Load any .el file in the packages.d directory
;; - Misc changes to get the initialization to complete without errors
;; - Removed copyright in header


;;;;;;
;;; Emacs 24 or greater only
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))


;;;;;;
;;; Initialize emacs features

;; Require all `.el' files in the init.d directory
(let ((dir (expand-file-name "init.d" user-emacs-directory)))
  (when (file-exists-p dir)
    (add-to-list 'load-path dir)
    (dolist (file (directory-files dir nil "\.el$"))
      (message "require file %s..." file)
      (require (intern (file-name-sans-extension file)) nil t))))

;; Load all `.el' files in the packages.d directory
(let ((dir (expand-file-name "packages.d" user-emacs-directory)))
  (when (file-exists-p dir)
    (mrb/load-directory dir)))


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
