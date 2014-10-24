;;; protect-buffers.el --- Protect system buffers

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

(message "protect-buffers -- protect the system buffers from being killed.")

(save-excursion
  (set-buffer "*scratch*")
  (emacs-lock-mode 'kill)
  (set-buffer "*Messages*")
  (emacs-lock-mode 'kill))

;;; protect-buffers.el
