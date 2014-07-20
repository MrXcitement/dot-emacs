(defun mrb:eshell-command-to_string (command)
  "Run the eshell command specified and return the results as a string."
  (with-temp-buffer
    (eshell-command command t)
    (buffer-string)))

(defun mrb:eshell-command-exist-p (command)
  "Determine if the specified command exists on this machine."
  (not (string-equal (mrb:eshell-command-to_string (concat "which " command)) "")))

(provide 'init-defuns)
