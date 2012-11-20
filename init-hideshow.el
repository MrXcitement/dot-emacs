;; init-hideshow.el -- initialize the hs minor mode

;; Mike Barker <mike@thebarkers.com>
;; November 19, 2012

;; Copyright (c) 2012 Mike Barker 

;; Change log:
;; 2012.11.19
;; * First release.

;;; setup selective display for hidding
(defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

(defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))

;;; global keymaps for toggling hiding
(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-=") 'toggle-selective-display)

;;; nxml-mode config
(add-to-list 'hs-special-modes-alist
        '(nxml-mode
          "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
          ""
          "<!--" ;; won't work on its own; uses syntax table
          (lambda (arg) (my-nxml-forward-element))
          nil))

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag)
	(outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b"))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
	  (nxml-forward-balanced-item 1)
	(error nil)))))


;;; hook into the following major modes
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'nxml-mode-hook       'hs-minor-mode)

(provide 'init-hideshow)
