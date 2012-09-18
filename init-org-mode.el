;;; 
;; Initialize the org mode package
;; See http://orgmode.org

;; Mike Barker <mike@thebarkers.com
;; August 6th, 2009

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

(message "Initializing org-mode...")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-6.34c/lisp") t)
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/Personal/Org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; We need to update the checksum file to only transfer updated files.
(add-hook 'org-mobile-post-push-hook
       (lambda () (shell-command "find ~/Dropbox/MobileOrg -name '*.org' -type f -print | sed 's/^\\.\\///' | xargs md5 > ~/Dropbox/MobileOrg/checksums.dat")))

;; Remember mode integration
(org-remember-insinuate)
(define-key global-map "\C-cr" 'org-remember)
(setq org-remember-templates
     '(
      ("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" (concat org-directory "/tasks.org") "Tasks")
      ("Note" ?n "\n* %^{topic} %T \n%i%?\n" (concat org-directory "/notes.org"))
      ("Journal" ?j "\n* %U \n%i%?\n" (concat org-directory "/journal.org"))
      ))

(setq org-refile-targets (quote (("notes.org" :level . 1)
				 ("someday.org" :level . 2)
				 ("tasks.org" :maxlevel . 1)
				 )))

;; Personal agenda files
;; ===================================================================
;; calendar.org       Birthdays, Aniverseries, etc  <-> Google Calendar
;; journal.org        Daily information             <-> Evernote
;; notes.org          Random notes                  <-> Evernote
;; someday.org        Tasks to be done someday      <-> RTM
;; tasks.org          Projects and tasks            <-> RTM
(setq org-agenda-files (list (concat org-directory "/calendar.org")
			     (concat org-directory "/journal.org")
			     (concat org-directory "/notes.org")
			     (concat org-directory "/someday.org")
			     (concat org-directory "/tasks.org")
			     ))

(provide 'init-org-mode)
;;; end init-org-mode
