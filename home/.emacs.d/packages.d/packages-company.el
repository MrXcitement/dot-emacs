;;; packages-company.el --- Install and configure the `company' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Use the complete any mode.

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.
;; 2015.05.15
;; * First release.

;;; Code:
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

(provide 'packages-company)
