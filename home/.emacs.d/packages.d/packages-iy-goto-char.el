;;; packages-iy-go-to-char --- Install and configure the `iy-go-to-char' package.

;; Mike Barker <mike@thebarkers.com>
;; May 15, 2015

;;; Commentary:
;; Provide the ability to go from point to the first occurence of a
;; specified character and then continue to the next and subsequent
;; occurences.
;; https://www.emacswiki.org/emacs/IyGoToChar

;;; History:
;; 2023.03.17
;; * rename and refactor this file into a valid package.

;;; Code:
(use-package iy-go-to-char
  :disabled
  :ensure t
  :bind
    (("C-c m" . iy-go-to-char)
     ("C-c M" . iy-go-to-char-backward)))

(provide 'packages-iy-goto-char)
