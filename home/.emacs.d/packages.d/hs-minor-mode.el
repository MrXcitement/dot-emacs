;;; hs-minor-mode.el --- initialize the hs minor mode

;; Copyright (C) 2014 Mike Barker

;; Author: Mike Barker <mike@thebarkers.com>
;; Created: October 23, 2014

;; This file is not part of GNU Emacs.

;;; History:
;; 2014.11.12
;; * removed loading message


;;; toggle hiding block on/off
;; will revert to using selective display if it fails
(defun mrb:toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (mrb:toggle-selective-display column)))

;;; toggle selective display of to the current column
(defun mrb:toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

;;; rules used to handle hiding nxml sections
(defun mrb:nxml-forward-sexp-func (pos)
  (mrb:nxml-forward-element))

(defun mrb:nxml-forward-element ()
  (let ((nxml-sexp-element-flag)
  	(outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b"))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
  	  (nxml-forward-balanced-item 1)
  	(error nil)))))



;;; Configure the hs-minor-mode package
(use-package hs-minor-mode
  :bind
  (("C-c =" . mrb:toggle-hiding)
   ("C-c +" . mrb:toggle-selective-display))

  :init
  (progn
    ;; hook into the following major modes
    (add-hook 'c-mode-common-hook   'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
    (add-hook 'java-mode-hook       'hs-minor-mode)
    (add-hook 'lisp-mode-hook       'hs-minor-mode)
    (add-hook 'perl-mode-hook       'hs-minor-mode)
    (add-hook 'sh-mode-hook         'hs-minor-mode)
    (add-hook 'nxml-mode-hook       'hs-minor-mode)
    (add-hook 'html-mode-hook       'hs-minor-mode))

  :config
  (progn
    ;; nxml-mode config to hide/show blocks
    (add-to-list 'hs-special-modes-alist
		 '(nxml-mode
		   "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
		   ""
		   "<!--"                        ; won't work on its own; uses syntax table
		   mrb:nxml-forward-sexp-func
		   nil                           ; mrb:nxml-hs-adjust-beg-func
		   ))

    ;; html-mode config to hide/show blocks
    (add-to-list 'hs-special-modes-alist
		 '(html-mode
		   "<!--\\|<[^/>]>\\|<[^/][^>]*"
		   "</\\|-->"
		   "<!--"
		   sgml-skip-tag-forward
		   nil))))

;;; hs-minor-mode.el ends here
