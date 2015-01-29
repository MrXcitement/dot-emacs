;;; init-c-mode.el --- Configure c-mode settings

;; Copyright (C) 2014 by Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; History:
;; 2014.10.23
;; * First release.


;;; hook c-mode to comment multi line regions like so:
;; /*
;;  * a multi-line
;;  * region comment
;;  */
(add-hook 'c-mode-hook
	  (lambda()
	    (setq comment-style 'extra-line)))

(provide 'init-c-mode)

;;; c-mode.el ends here.
