;; init-os.el -- Initialize os specific settings.

;; Mike Barker <mike@thebarkers.com>
;; February 27, 2014

;; Copyright (c) 2014 Mike Barker

;; Change log:
;; 2014.02.27
;; * First release.

(message "init-os -- Initialize operating system specific settings.")

(cond
 ;; Darwin (Mac OS X) custimzation
 ((eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  )
 ;; Linux customization
 ((eq system-type 'gnu/linux)
  )
 ;; Windows customizations
 ((eq system-type 'windows-nt)))

(provide 'init-os)
