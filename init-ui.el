;;; 
;; init-ui.el
;; Initialize the user interface.
;; This file is NOT a part of GNU Emacs

;; Mike Barker <mike@thebarkers.com>
;; July 1, 2012

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

;; July 1, 2012
;; - Renamed from init-gui.el to init-ui.el
;; - Refactored window-system checks to use when and when not.
;; 2012.09.16 MRB
;; - Added turn on column mode
;; - Added turn on paren mode
;; 2012.09.17 MRB
;; * Turn on highlight-80+-mode, highlights lines > 80

(message "Loading init-ui.el...")

(setq inhibit-splash-screen t)
(blink-cursor-mode -1)
(column-number-mode t)
(show-paren-mode t)
(highlight-80+-mode t)

;; This is a window sytem (a gui) session
(when (window-system)
  
  (load-theme 'tango t) ; Set the default color theme

  ;;; 
  ;; Use hl-line-mode to highlight the current line.
  ;;(global-hl-line-mode)
  ;;(set-face-background 'hl-line "lightyellow")

  ;; Determine what type of system we are on
  (cond

   ;; Darwin (Mac OS X) custimzations
   ((eq system-type 'darwin)
    (set-face-font 'default "Droid Sans Mono Dotted 14") ; Set default font
    )

   ;; Gnu/linux customizations
   ((eq system-type 'gnu/linux)
    )

   ;; Windows customizations
   ((eq system-type 'windows-nt)
    (set-face-font 'default "Consolas 12") ; Set default font
    )

   ) ; end cond
  ) ; end when (window-system)

;; This is not a window system (a terminal) session
(when (not (window-system))
  (menu-bar-mode -1)
  ) ; end (when (not (window-system))

(provide 'init-ui)
;;; init-ui.el ends here
