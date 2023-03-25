;;; init-ui-linux.el --- initialze the user interface on Linux systems

;; Mike Barker <mike@thebarkers.com>
;; March 25th, 2023

;;; Commentary:
;; Initialize the user interface on Linux systems

;;; History:
;; 2023-03-25 MRB
;; * Created

;;; Code:

;; Linux customization
(when (eq system-type 'gnu/linux)
  ;; GUI customization
  (when (window-system)
    ;; Font customization
    (when (member "Monospace" (font-family-list))
      (set-face-font 'default "Monospace 11"))))

(provide 'init-ui-linux)
;;; End of init-ui-linux
