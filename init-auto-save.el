;;;
;; Configure autosave and backup files
;; This file is NOT part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>

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

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html
;; this puts files in the /var/folders/*/*/-Tmp-/emacs-[user-login-name]/

;; MRB 2009-08-18 - Failing under cygwin due to invalid DOS filename chars
;; modified to no longer use invalid characters.

;; MRB 2009-09-02 - Moved backup and autosave files under ~/tmp/emacs folder
;; 2012.09.18 MRB
;; Changed auto save file prefix from .auto-save- to auto-saves-

(message "Initialize where backup and auto-save files are stored.")

(defvar user-temporary-file-directory (expand-file-name "~/tmp/emacs/"))
(make-directory user-temporary-file-directory t)

;; Backup files
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

;; Auto save files
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory "auto-saves-"))

(setq auto-save-file-name-transforms
      `(("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,user-temporary-file-directory t)))

(provide 'init-auto-save)
;;; End init-auto-save.el
