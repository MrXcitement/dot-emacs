;;;
;; Load the haskel mode file
(autoload 'haskel-mode "haskel-mode"
     "Mode for editing haskel source files")

;; Setup indenting
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Syntax coloring
(add-hook 'haskell-mode-hook 'font-lock-mode)

;; Haskel menu items
(global-set-key [(control meta down-mouse-3)] 'imenu)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(provide 'init-haskell)