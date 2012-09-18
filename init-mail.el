;; init-mail.el
;; Initialize emacs for email ussage
;; Mike Barker <mike@thebarkers.com>
;; [21.01.2010 - 10:59]

;; Setup primary news group server
(setq gnus-select-method
      '(nntp "news.gmane.org"
	     (nntp-open-ssl-stream)))

;; Setup viewing my imap mail accounts hosted by google
(add-to-list 'gnus-secondary-select-methods 
      '(nnimap "mike@thebarkers.com"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-authinfo-file "~/.auth/imap.gmail.com-mike_thebarkers_com")
	       (nnimap-stream ssl)))

;; (setq gnus-select-method '(nnimap "imap.gmail.com"
;;            (nnimap-address "imap.gmail.com")
;;            (nnimap-server-port 993)
;;            (nnimap-authinfo-file "~/.imap-authinfo")
;;            (nnimap-stream ssl)))

;; Setup sending mail through google smtp server
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.auth/smtp.gmail.com-mike_thebarkers_com.auth")
      smtpmail-default-smtp-server "smtp.gmail.com-mike_thebarkers_com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
(require 'smtpmail)

(provide 'init-mail)
