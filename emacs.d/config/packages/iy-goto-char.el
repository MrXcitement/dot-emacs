;;; iy-go-to-char:

;; Provide the ability to go from point to the first occurence of a
;; specified character and then continue to the next and subsequent
;; occurences.

(use-package iy-go-to-char
  :ensure t
  :bind
    (("C-c m" . iy-go-to-char)
     ("C-c M" . iy-go-to-char-backward)))
