;;; packages-icomplete.el --- Configure the `icomplete' package.

;; Mike Barker <mike@thebarkers.com>
;; March 23rd, 2023

;;; Commentary:
;; The `icomplete' package implements a more fine-grained minibuffer
;; completion feedback scheme.  Prospective completions are concisely
;; indicated within the minibuffer itself, with each successive
;; keystroke.

;;; History:
;; 2023.03.17
;; * Created

;;; Code:


(use-package icomplete
  :config
  (progn
    (when (version< emacs-version "28.1")
      (icomplete-mode 1)
      ;; show choices vertically
      (setq icomplete-separator "\n")
      (setq icomplete-hide-common-prefix nil)
      (setq icomplete-in-buffer t)
      (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
      (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions))
    (unless (version< emacs-version "28.1")
      ;; An enhanced ‘icomplete-mode’ that emulates ‘ido-mode’.  This global
      ;; minor mode makes minibuffer completion behave more like ‘ido-mode’
      ;; than regular ‘icomplete-mode’.
      ;; Only available in Emacs 28.1+
      (fido-vertical-mode 1))))

(provide 'packages-icomplete)
