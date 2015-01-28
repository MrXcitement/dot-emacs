;;; buffer-move:

;; Bind the Ctrl-C and Shift-Arrow Keys to swap the current buffer with
;; the buffer in the window that the arrow key pressed points at.

(use-package buffer-move
  :ensure t
  :bind (("C-c <S-up>" . buf-move-up)
	 ("C-c <S-down>" . buf-move-down)
	 ("C-c <S-left>" . buf-move-left)
	 ("C-c <S-right>" . buf-move-right)))
