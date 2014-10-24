;;; ab-helper.el --- helper functions and macros.

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

(defun mrb:eshell-command-to_string (command)
  "Run the eshell command specified and return the results as a string."
  (with-temp-buffer
    (eshell-command command t)
    (buffer-string)))

(defun mrb:eshell-command-exist-p (command)
  "Does a specified command exist on this machine."
  (not (string-equal (mrb:eshell-command-to_string (concat "which " command)) "")))

;;; ab-helper.el ends here.
