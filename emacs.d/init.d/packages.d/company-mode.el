;;; company-mode.el

;;; Use the complete any mode.
(use-package company
  :disabled t
  :ensure nil

  :demand t 				; make sure this is loaded at startup, not defered.
  :bind (("C-c SPC" . company-complete)	; key to force completion
	 ("C-c /" . company-files))	; key to force file completion

  :config
  (progn
    (global-company-mode)
    (setq company-tooltip-limit 20)                      ; bigger popup window
    (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
    (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
    (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
    ))
