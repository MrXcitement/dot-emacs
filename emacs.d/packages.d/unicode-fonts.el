;;; unicode-fonts.el --- install and configure the unicode-fonts module

;; provide emoticon support in emacs. very important indeed.

(use-package unicode-fonts
  :ensure t
  :config
  (require 'unicode-fonts)
  ;; (setq unicode-fonts-block-font-mapping
  ;; 	'(("Emoticons"
  ;; 	   ("Apple Color Emoji" "Symbola" "Quivira")))
  ;; 	unicode-fonts-fontset-names '("fontset-default"))
  ;; Add Apple Emoji support
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
  (unicode-fonts-setup))
