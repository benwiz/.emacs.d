(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0))                               ; Disable the tooltips

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs nil ; 'yes-or-no-p            ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 5                                  ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace t                       ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 column-number-mode t                             ; Display column numbers
 line-spacing 1                                   ; Add N pixel below each line
 truncate-lines t                                 ; Truncate long lines
 )
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 0)                                 ; Disable the menu bar
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encodingo
(global-display-line-numbers-mode)                ; Display line numbers
(show-paren-mode)                                 ; Show matching parenthesis
(desktop-save-mode 0)                             ; Don't save buffer and window state
(display-battery-mode 0)

;; ;; Shut off mouse clicks because my work computer's trackpad is super annoying, resizing windows with mouse still works
;; (global-unset-key (kbd "<down-mouse-1>"))
;; (global-unset-key (kbd "<mouse-1>"))
;; (global-unset-key (kbd "<down-mouse-3>"))
;; (global-unset-key (kbd "<mouse-3>"))

;;(if (eq window-system 'ns)
;;  (add-to-list 'default-frame-alist '(maximized .))
;;  (add-to-list 'default-frame-alist '(fullscreen .)))

;; uncomment to start in full screen:
;; (add-hook 'after-make-frame-functions
;;         (lambda (frame)
;;           (set-frame-parameter frame 'fullscreen 'fullboth)
;;           (tool-bar-mode 0)
;;           (scroll-bar-mode 0)))
;; (set-frame-parameter nil 'fullscreen 'fullboth)

(add-hook 'focus-out-hook #'garbage-collect)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(setq desktop-load-locked-desktop t
      desktop-restore-forces-onscreen nil)
