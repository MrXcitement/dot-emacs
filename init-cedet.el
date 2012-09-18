;;; 
;; init-cedet.el
;; Initialize the cedet package system
;; This file is NOT a part of GNU Emacs

;;  <mike@thebarkers.com>
;; September 15, 2012

;;; License

;; Copyright (c) 2012 Mike Barker

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Change log:
;; 2012.09.15
;;      * First released.

;; See: http://www.emacswiki.org/emacs/CollectionOfEmacsDevelopmentEnvironmentTools

;;; MRB - Emacs 23.2 now has cedit installed
;;(load-file "~/.emacs.d/site-lisp/cedet-cvs/common/cedet.el")

(message "Loading the cedet package...")

;;;
;; Turn on EDE (Project handling mode)
(global-ede-mode t)
(semantic-mode t)

;; Semantic's customization 
;; Depending on your requirements, you can use one of the commands, described
;; below, to load corresponding set of features (these commands are listed in
;; increasing order, and each command include features of previous commands):

;; Enables only minimum of necessary features — keep syntactic information for
;; current buffer up-to date, storing of syntactic information for later use
;; (Semanticdb), and loading of corresponding information with Semanticdb and
;; Ebrowse
;;(semantic-load-enable-minimum-features)

;; Enables senator-minor-mode for navigation in buffer,
;; semantic-mru-bookmark-mode for storing positions of visited tags, and
;; semantic-idle-summary-mode, that shows information about tag under point
;;(semantic-load-enable-code-helpers)

;; Enables semantic-stickyfunc-name that displays name of current function in
;; topmost line of buffer, semantic-decoration-mode to decorate tags, using
;; different faces, and semantic-idle-completion-mode for automatic generation of
;; possible names completions, if user stops his work for some time;
;;(semantic-load-enable-gaudy-code-helpers)

;; Enables which-func-mode, that shows name of current function in status line;
;;(semantic-load-enable-excessive-code-helpers)

;; Enables several modes, that are useful when you debugging Semantic —
;; displaying of parsing errors, its state, etc.
;;(semantic-load-enable-semantic-debugging-helpers) ; Debugging help                 

;;;
;; Store semantic.cache files in a semantic under the .emacs.d
;;(setq semanticdb-default-save-directory "~/.emacs.d/semantic")
;;(setq semantic-load-turn-useful-things-on t)

;;???
;;(require 'semantic-decorate-include)

;; smart complitions
;;(require 'semantic-ia)

;; gcc setup
;;(require 'semantic-gcc)

;;; ede customization
;;(require 'semantic-lex-spp)


;;;
;; srecode templating support
;;(global-srecode-minor-mode 1)	      ; Enable template insertion menu

;;(global-semantic-tag-folding-mode)

(provide 'init-cedet)
;;; init-cedet.el ends here
