;;;
;; Filename: init-org-feed.el
;; Description: Initialize the org-feed mode
;; Author: Mike Barker <mike@thebarkers.com>
;; Maintainer: Mike Barker <mike@thebarkers.com>
;; Copyright (C) 2010, Mike Barker, all rights reserved.
;; Created: 2010-03-11 11:00:33
;; Version: 0.1
;; Last-Updated: 2010-03-11 11:00:33
;;           By: Mike Barker

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Change log:
;;	
;; 2010/03/11
;;      * First released.
;; 

;;; Acknowledgements:
;;

;;; TODO
;;

;;; Require
;;

;;; Code:

(setq org-feed-alist
      '(("Remember The Milk"
         "https://www.rememberthemilk.com/atom/mrbarker/"
         "~/Dropbox/Personal/Org/rtm.org" "Remember The Milk Entries"
	 :parse-feed org-feed-parse-atom-feed
	 :parse-entry org-feed-parse-rtm-entry
	 )))

(defun org-feed-parse-rtm-entry (entry)
  "Parse the `:item-full-text' as a sexp and create new properties."
  (let ((xml (car (read-from-string (plist-get entry :item-full-text)))))
    ;; Get first <link href='foo'/>.
    (setq entry (plist-put entry :link
                           (xml-get-attribute
                            (car (xml-get-children xml 'link))
                            'href)))
    ;; Add <title/> as :title.
    (setq entry (plist-put entry :title
                           (car (xml-node-children
                                 (car (xml-get-children xml 'title))))))
    (let* ((content (car (xml-get-children xml 'content)))
           (type (xml-get-attribute-or-nil content 'type)))
      (when content
        (cond
         ((string= type "text")
          ;; We like plain text.
          (setq entry (plist-put entry :description (car (xml-node-children content)))))
         ((string= type "html")
          ;; TODO: convert HTML to Org markup.
          (setq entry (plist-put entry :description (car (xml-node-children content)))))
         ((string= type "xhtml")
          ;; TODO: convert XHTML to Org markup.
          (setq entry (plist-put entry :description (prin1-to-string (xml-node-children content)))))
         (t
          (setq entry (plist-put entry :description (format "Unknown '%s' content." type)))))))
    entry))

(provide 'init-org-feed)

;;; init-org-feed.el ends here

