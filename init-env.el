;;; 
;; init-env.el
;; Initialize the system environment
;; Under Mac OS X Mountain Lion, you can no longer easily set the environment for
;; applications not run from a shell. I needed a way to tell emacs where to look
;; for things without resorting to letting every other app access a changed path.
;; By setting exec-path it looks like I can resolve this problem when initializing
;; Emacs.
;; http://www.ergoemacs.org/emacs/emacs_env_var_paths.html

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

;; Determine the system we are on
(cond
 ;; Darwin (Mac OS X) custimzations
 ((string-equal "darwin" system-type)
  (require 'init-env-darwin nil t)
  )

 ;; Gnu/linux customizations
 ((string-equal "gnu/linux" system-type)
  (require 'init-env-linux nil t)
  )

 ;; Windows customizations
 ((string-equal "windows-nt" system-type )
  (require 'init-env-windows nil t)
  )
)

(provide 'init-env)
;;; init-env.el ends here
