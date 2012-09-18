;;; 
;; Filename: init-irbsh.el
;; Description: Initialize the interactive ruby shell irbsh
;; Author: Mike Barker <mike@thebarkers.com>
;; Maintainer: Mike Barker <mike@thebarkers.com>
;; Copyright (C) 2010, Mike Barker, all rights reserved.
;; Created: 2010-02-18 10:12:19
;; Version: 0.1
;; Last-Updated: 2010-02-18 10:12:19
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
;; 2010/02/18
;;      * First released.
;; 

;;; Acknowledgements:
;;

;;; TODO
;;

;;; Require
;;

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/irbsh")
(load "irbsh")
(load "irbsh-toggle")

(provide 'init-irbsh)

;;; init-irbsh.el ends here
