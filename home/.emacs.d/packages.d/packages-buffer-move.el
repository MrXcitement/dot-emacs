;;; packages-buffer-move.el --- Install and configure the buffer-move package

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Bind the Ctrl-C and Shift-Arrow Keys to swap the current buffer with
;; the buffer in the window that the arrow key pressed points at.

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; * rename personal functions from mrb:funcname to my/funcname
;; 2015.05.15
;; * First release.

;;; Code:
(use-package buffer-move
  :ensure t
  :bind (("C-c <S-up>" . buf-move-up)
	 ("C-c <S-down>" . buf-move-down)
	 ("C-c <S-left>" . buf-move-left)
	 ("C-c <S-right>" . buf-move-right)))

(provide 'packages-buffer-move)
