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

;; Ido - interactive do - switches between buffers and opens files and
;; directories with a minimum of keystrokes.
(use-package ido
  :if (version< emacs-version "28.1")
  :config
  (progn
    (ido-mode 1)
    ;; show choices vertically
    (setf (nth 2 ido-decorations) "\n")
    ;; show any name that has the chars you typed
    (setq ido-enable-flex-matching t)
    ;; use current pane for newly opened file
    (setq ido-default-file-method 'selected-window)
    ;; use current pane for newly switched buffer
    (setq ido-default-buffer-method 'selected-window)))

(provide 'packages-ido)
