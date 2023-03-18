;;; packages-ido.el --- Configure the `ido' package.

;; Mike Barker <mike@thebarkers.com>
;; October 23, 2014

;;; Commentary:
;; The ido.el package by KimStorm lets you interactively do things
;; with buffers and files. As an example, while searching for a file
;; with C-x C-f, ido can helpfully suggest the files whose paths are
;; closest to your current string, allowing you to find your files
;; more quickly.
;; https://www.emacswiki.org/emacs/InteractivelyDoThings

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2014.11.12
;; * removed loading message

;;; Code:
(use-package ido
  :config
  (progn
    (setq ido-enable-flex-matching t) ; enable fuzzy matching
    (setq ido-everywhere t)
    (ido-mode 1)))

(provide 'packages-ido)
