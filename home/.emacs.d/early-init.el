;;; early-init.el --- Early initialization

;; Mike Barker <mike@thebarkers.com>
;; March 24th, 2023

;;; Commentary:
;; I have added this file in an attempt to speed up Emacs' startup time.
;; Tweak garbage collector settings to decrease startup time, saved .5 seconds.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Init-File.html#index-early-init-file
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html

;;; History:
;; 2023-03-25
;; * Created

;;; Code:

;; Increase GC threshold during startup to 50MB
(setq gc-cons-threshold (* 50 1000 1000))

;; Turn off ui elements
(setq inhibit-splash-screen t)
(tool-bar-mode -1)

;; After emacs has started...
;; Tell us how long it took to start and how many times the GC ran
;; Reset the GC threshold to 8KB
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (setq gc-cons-threshold (* 800 1000))))

(provide 'early-init)
;;; End of early-init.el
