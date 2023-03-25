;;; init-ui-windows.el --- initialze the user interface on Windows systems

;; Mike Barker <mike@thebarkers.com>
;; March 25th, 2023

;;; Commentary:
;; Initialize the user interface on Windows systems

;;; History:
;; 2023-03-25 MRB
;; * Created

;;; Code:

;; Windows customization
(when (eq system-type 'windows-nt)
  ;; GUI customization
  (when (window-system)
    ;; Font customization
    (when (member "Lucida Console" (font-family-list))
      (set-face-font 'default "Lucida Console 10"))))

(provide 'init-ui-windows)
;;; End of init-ui-windows
