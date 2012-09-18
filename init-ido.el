;;;
;; Filename: init-ido.el
;; Description: Initialize the ido minor mode (Interactive IDO mode)
;; Author: Mike Barker <mike@thebarkers.com>
;; Maintainer: Mike Barker <mike@thebarkers.com>
;; Copyright (C) 2010, Mike Barker, all rights reserved.
;; Created: 2010-02-09 13:22:12

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; M-x mode
(defun ido-execute-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp)))))

(global-set-key (kbd "M-x") 'ido-execute-command)

(provide 'init-ido)

;;; init-ido.el ends here
