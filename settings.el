(defconst *is-a-mac* (eq system-type 'darwin))
(when *is-a-mac*
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(setq mac-command-modifier 'meta)

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun load-init-el ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i") 'load-init-el)

;; See top of file for eval that updates the html file
(use-package htmlize)
(setq org-html-validation-link "")


;; FIXME there is a bug here, these endlessly appends to the head, it need to not do this forever
(defun org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
  background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq org-html-head-extra
            (concat org-html-head-extra
                    (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                            my-pre-bg my-pre-fg)
                    (format "<style type=\"text/css\">\n pre.src-emacs-lisp:before {background-color: %s; color: %s;}</style>\n"
                            my-pre-fg my-pre-bg))))))

(add-hook 'org-export-before-processing-hook 'org-inline-css-hook)

;; Shut off mouse clicks because my work computer's trackpad is super annoying, resizing windows with mouse still works
(global-unset-key (kbd "<down-mouse-1>"))
(global-unset-key (kbd "<mouse-1>"))
(global-unset-key (kbd "<down-mouse-3>"))
(global-unset-key (kbd "<mouse-3>"))

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
;; (cd "~/code/")                                    ; Move to the user directory
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

;(if (eq window-system 'ns)
;  (add-to-list 'default-frame-alist '(maximized .))
;  (add-to-list 'default-frame-alist '(fullscreen .)))
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame 'fullscreen 'fullboth)
            (tool-bar-mode 0)
            (scroll-bar-mode 0)))
(set-frame-parameter nil 'fullscreen 'fullboth)

(add-hook 'focus-out-hook #'garbage-collect)
(display-battery-mode 0)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)

(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Fetch pacakges when package-archive-contents does not exist
(when (not package-archive-contents)
  (package-refresh-contents))

;; List packages to install
(setq package-list '(use-package))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Suggest to upgrade packages occasionally, TODO would be better to ask the first time emacs is opened each month
(when (eq 0 (random 50))
  (when (y-or-n-p-with-timeout "Do you want to check packages for upgrades? " 6 nil)
    (package-list-packages)
    (message "Run `Shift-U x` to upgrade")))

;; force packages to always be installed, no need to defer with emacsclient, I think
;; NOTE must set `:ensure nil` if not a package.el package, like dired
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; To upgrade manually `M-x list-packages U x`, if that is not convenient check below stackoverflow post for some helper functions
;; https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
;; Alternatively use auto-package-update but that caused surprise issues in the past when upgrading blindly

(use-package gnu-elpa-keyring-update)

(if *is-a-mac*
    (add-to-list 'custom-theme-load-path "/Users/benwiz/.emacs.d/themes")
  (add-to-list 'custom-theme-load-path "/home/benwiz/.emacs.d/themes"))

;; emacs27 loads themes immediately, this line prevents that. Presumably there is a good reason
;; for that and I should figure out a workaround. Also, this is a private variable so may change
;; or go away in the future.
(setq custom--inhibit-theme-enable nil)

;; Underline/highlight selected line
(global-hl-line-mode 1)

;; Light theme. I like the default theme more than any other light theme I found.
;; The following are global customizations I intend to apply to the default theme. There could be a more constrained way which would be better.
;; TODO need to set this gray font
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 135)
(set-face-attribute 'hl-line nil :background "#e3ffe3")
(set-face-attribute 'region nil :background "#E4E4E4")
(set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-constant-face nil :foreground "#255814") ;; Forest Green is default; DarkGreen is good; hex is darker forest green
(set-face-attribute 'font-lock-type-face nil :foreground "#006060") ;; DarkCyan (#008b8b) is default, hex is darker version
(set-face-attribute 'trailing-whitespace nil :background "#e0eeff")

;; Dark theme
(defun load-spolsky ()
  "Load Spolsky."
  (load-theme 'spolsky t)
  (custom-theme-set-faces
   'spolsky
   `(default ((t (:foreground "#F2F2F2" :background "#161A1F"))))
   `(hl-line ((t (:background "#1E252F" :underline nil))))
   `(font-lock-comment-delimiter-face ((t (:foreground "#8C8C8C" :slant italic))))
   `(font-lock-comment-face ((t (:foreground "#8C8C8C" :slant italic))))
   `(trailing-whitespace ((t (:background "#5a708c"))))
   `(lsp-face-highlight-textual ((t (:background "#353535")))) ;; "#323E30" ;; "#555" is same as selection color, the other one is half way between hl-line and trailing-whitespace
   `(org-level-4 ((t (:foreground "#EEEEBF"))))
   ))

;; Start in spolsky
(add-hook 'after-make-frame-functions (lambda (frame) (load-spolsky)))
(load-spolsky)

;; Toggle themes
(defun toggle-theme ()
  "Toggle Spolsky theme on and off."
  (interactive)
  (if (member 'spolsky custom-enabled-themes)
      (disable-theme 'spolsky)
    ;; (enable-theme 'spolsky) ;; This should work (and did work I think) because spolsky has been loaded before
    (load-spolsky)))
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10>") 'toggle-theme)

;; Modeline
(use-package all-the-icons)
(use-package doom-modeline
  ;; NOTE Must run `M-x all-the-icons-install-fonts` to install icons
  ;; https://github.com/seagle0128/doom-modeline#customize
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 20)
  ;; (setq doom-modeline-persp-name t)
  ;; (setq doom-modeline-display-default-persp-name t)
  (setq doom-modeline-env-version t)
  )

(use-package pomodoro
  :config
  (defun pomodoro-add-to-mode-line* ()
    "My version of pomodoro-add-to-mode-line"
    (if (not (member '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format))
        (setq-default mode-line-format (cons '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format)))
    ;; For development, removing it from list is helpful
    ;; (setq-default mode-line-format (remove '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format))
    )
  (pomodoro-add-to-mode-line*)
  )

(use-package redtick)

(if (file-exists-p "~/.emacs.d/emacs.env")
    (use-package load-env-vars
      :init
      (load-env-vars "~/.emacs.d/emacs.env")))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(require 'misc)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "C-i"))
(global-unset-key (kbd "C-x C-x"))
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-x C-u"))

(global-set-key (kbd "C-x k") 'kill-this-buffer) ;; Don't ask which buffer, just do it
(global-set-key (kbd "C-x C-x") 'mode-line-other-buffer)
(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c n") 'narrow-to-defun)
(global-set-key (kbd "C-c w") 'widen)
(global-set-key (kbd "C-l") 'recenter)

(use-package dired
  :ensure nil ;; dired is not package.el
  :config
  (setq dired-omit-files "^.~$")

  ;; dired - reuse current buffer by pressing 'a'
  ;; (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (require 'dired-x)
  (add-hook 'dired-mode-hook 'dired-omit-mode))

;; no lines in docview (actually i think it is off by default, the number is from something else) (pdf viewer)
;; (add-hook 'doc-view-minor-mode-hook (lambda () (linum-mode 0)))

(if (and (file-directory-p "~/code/bela-mode.el/")
         (file-directory-p "~/code/Bela/scripts/"))
 (use-package bela-mode
   :load-path "~/code/bela-mode.el"
   :init (setq bela-scripts-dir "~/code/Bela/scripts/")))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link))

(use-package switch-buffer-functions) ;; although this is not explicitly git, my only use case currently is diff-hl
(use-package diff-hl
  :after (switch-buffer-functions)
  :config
  ;; do not use diff-hl-flydiff-mode for fear of speed issues
  (diff-hl-margin-mode)
  (add-hook 'switch-buffer-functions (lambda (prev curr) (diff-hl-update))) ;; update diff when switching buffers
  (global-diff-hl-mode))

(use-package restart-emacs)
(use-package dictionary)
(use-package wgrep) ;; edit file in grep buffer
;; (use-package itail) ;; tail file within emacs

(use-package scratch
  :bind (("C-c s" . scratch)))

(use-package fic-mode
  :init
  (defface fic-face
    '((((class color))
       (:foreground "orange" :weight bold :slant italic))
      (t (:weight bold :slant italic)))
    "Face to fontify FIXME/TODO words"
    :group 'fic-mode)
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; (use-package restclient
;;   :mode ("\\.http\\'" . restclient-mode))

;; (use-package free-keys
;;   :bind ("C-h C-k" . 'free-keys))

;; (use-package dashboard
;;     ;; https://github.com/emacs-dashboard/emacs-dashboard ;
;;     :ensure t
;;     :init
;;     ;; Banner and title and footer
;;     (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard"
;;           dashboard-startup-banner 2 ;; 'official, 'logo, 1, 2, 3, or a path to img
;;           dashboard-center-content nil
;;           dashboard-show-shortcuts t
;;           dashboard-set-navigator t ;; Idk what this does, I think it isn't working
;;           dashboard-set-init-info t
;;           ;; dashboard-init-info "This is an init message!" ;; Customize init-info
;;           dashboard-set-footer t
;;           ;; dashboard-footer-messages '("Dashboard is pretty cool!") ;; Customize footer messages
;;           )
;;     ;; Widgets
;;     (setq dashboard-items '((recents  . 5)
;;                             (bookmarks . 5)
;;                             (projects . 5)
;;                             (agenda . 5)
;;                             (registers . 5))
;;           dashboard-set-heading-icons nil
;;           dashboard-set-file-icons nil)
;;     :config
;;     (dashboard-setup-startup-hook)
;;     ;; Custom widget
;;     ;; Ideas: weather, widget dedicated to each of my projects, news
;;     (defun dashboard-insert-custom (list-size)
;;       (insert "Custom text"))
;;     (add-to-list 'dashboard-item-generators '(custom . dashboard-insert-custom))
;;     (add-to-list 'dashboard-items '(custom) t)
;;     (defun dashboard ()
;;       "Open dashboard."
;;       (interactive)
;;       (switch-to-buffer "*dashboard*")
;;       (dashboard-refresh-buffer)))

;; (use-package exwm
;;   :config
;;   (require 'exwm-config)
;;   (exwm-config-default)

;;   ;; TODO what I really need to do is simulation keymaps for every application (mainly firefox)
;;   ;; (setq exwm-input-simulation-keys
;;   ;;   '(([?\C-b] . [left])
;;   ;;     ([?\C-f] . [right])
;;   ;;     ([?\C-p] . [up])
;;   ;;     ([?\C-n] . [down])
;;   ;;     ([?\C-a] . [home])
;;   ;;     ([?\C-e] . [end])
;;   ;;     ([?\M-v] . [prior])
;;   ;;     ([?\C-v] . [next])
;;   ;;     ([?\C-d] . [delete])
;;   ;;     ([?\C-k] . [S-end delete])))

;;   ;; (defun fhd/exwm-input-line-mode ()
;;   ;;   "Set exwm window to line-mode and show mode line"
;;   ;;   (call-interactively #'exwm-input-grab-keyboard)
;;   ;;   ;; (exwm-layout-show-mode-line)
;;   ;;   )

;;   ;; (defun fhd/exwm-input-char-mode ()
;;   ;;   "Set exwm window to char-mode and hide mode line"
;;   ;;   (call-interactively #'exwm-input-release-keyboard)
;;   ;;   ;; (exwm-layout-hide-mode-line)
;;   ;;   )

;;   ;; (defun fhd/exwm-input-toggle-mode ()
;;   ;;   "Toggle between line- and char-mode"
;;   ;;   (interactive)
;;   ;;   (with-current-buffer (window-buffer)
;;   ;;     (when (eq major-mode 'exwm-mode)
;;   ;;       (if (equal (second (second mode-line-process)) "line")
;;   ;;           (progn
;;   ;;             (fhd/exwm-input-char-mode)
;;   ;;             (message "Input mode on"))
;;   ;;         (progn
;;   ;;           (fhd/exwm-input-line-mode)
;;   ;;           (message "Line mode on"))))))

;;   ;; (defun fhd/toggle-exwm-input-line-mode-passthrough ()
;;   ;;   "Toggle line mode pass through. Really probably dont' need to toggle this much. Keep in first form."
;;   ;;   (interactive)
;;   ;;   (if exwm-input-line-mode-passthrough
;;   ;;       (progn
;;   ;;         (setq exwm-input-line-mode-passthrough nil)
;;   ;;         (message "App receives all the keys now (with some simulation)"))
;;   ;;     (progn
;;   ;;       (setq exwm-input-line-mode-passthrough t)
;;   ;;       (message "emacs receives all the keys now")))
;;   ;;   ;; Enable this to update modeline if I add a flag for passthrough, otherwise don't need to force update modeline
;;   ;;   ;; (force-mode-line-update)
;;   ;;   )

;;   ;; (exwm-input-set-key (kbd "s-w") 'fhd/exwm-input-toggle-mode) ;; NOTE some keybindings just don't work (like s-i or s-p)
;;   ;; ;; (exwm-input-set-key (kbd "s-p") 'fhd/toggle-exwm-input-line-mode-passthrough) ;; but s-p does work here

;;   ;; ;; close wm buffer
;;   ;; ;; (kill-buffer "wm")

;;   ;; (require 'exwm-randr)
;;   ;; (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
;;   ;; (add-hook 'exwm-randr-screen-change-hook
;;   ;;           (lambda ()
;;   ;;             (start-process-shell-command
;;   ;;              "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
;;   ;; (exwm-randr-enable)

;;   ;; ;; TODO I think I can (should) delete the "wm" buffer
;;   ;; (defun wm-xmodmap()
;;   ;;   (call-process "xmodmap" nil (get-buffer-create "wm") nil
;;   ;;                 (expand-file-name "~/.config/xmodmap")))
;;   ;; (wm-xmodmap)
;;   )

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  ;; (require 'mc-hide-unmatched-lines-mode) ;; Idk why this was here, delete if it's been a while
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

(use-package swiper
  :init
  (set-face-attribute 'isearch nil :background "#FF9F93")
  :config
  (global-set-key (kbd "M-i") 'swiper-isearch))

(defun swiper--from-isearch ()
  "Invoke `swiper' from isearch.
     https://github.com/ShingoFukuyama/helm-swoop/blob/f67fa8a4fe3b968b7105f8264a96da61c948a6fd/helm-swoop.el#L657-668 "
  (interactive)
  (let (($query (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (isearch-exit)
    (swiper $query)))
(define-key isearch-mode-map (kbd "M-i") 'swiper--from-isearch)

(use-package counsel
  :config
  ;; tons more suggested key bindings here https://oremacs.com/swiper
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  (global-set-key (kbd "C-c c") 'counsel-compile)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-i") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c a") 'counsel-linux-app)
  )

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(defun mc-mark-next-like-this-then-cycle-forward (arg)
  "Mark next like this then cycle forward, take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/mark-next-like-this)
  (call-interactively 'mc/cycle-forward))

(defun mc-skip-to-next-like-this-then-cycle-forward (arg)
  "Skip to next like this then cycle forward, take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/cycle-backward)
  (call-interactively 'mc/skip-to-next-like-this)
  (call-interactively 'mc/cycle-forward))

(defun mc-mark-previous-like-this-then-cycle-backward (arg)
  "Mark previous like this then cycle backward take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/mark-previous-like-this)
  (call-interactively 'mc/cycle-backward))

(defun mc-skip-to-previous-like-this-then-cycle-backward (arg)
  "Skip to previous like this then cycle backward take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/cycle-forward)
  (call-interactively 'mc/skip-to-previous-like-this)
  (call-interactively 'mc/cycle-backward))

(use-package multiple-cursors
  :bind (("C->" . mc-mark-next-like-this-then-cycle-forward)
         ("C-M->" . mc-skip-to-next-like-this-then-cycle-forward)
         ("C-<" . mc-mark-previous-like-this-then-cycle-backward)
         ("C-M-<" . mc-skip-to-previous-like-this-then-cycle-backward)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         )
  :config
  ;; By default, <return> exits mc ;; TODO FIXME
  (define-key mc/keymap (kbd "<return>") nil))

;; (use-package term
;;   :config
;;   ;; NOTE: After changing the following regexp, call `term-mode' in the term
;;   ;; buffer for this expression to be effective; because the term buffers
;;   ;; make a local copy of this var each time a new term buffer is opened or
;;   ;; `term-mode' is called again.
;;   (setq term-prompt-regexp ".*:.*>.*? "))

;; (use-package multi-term
;;   :config
;;   ;; TODO need to chang blue color to another color. I could look into `dircolors -b` but there may be an easier way.
;;   (setq term-bind-key-alist
;;         '(("C-c C-c" . term-interrupt-subjob)            ; default
;;           ("C-c C-e" . term-send-esc)                    ; default
;;           ;; ("C-c C-j" . term-line-mode) ;; TODO can I use the same command as EXWM?
;;           ;; ("C-c C-k" . term-char-mode) ;; TODO can I use the same command as EXWM?
;;           ("C-a"     . term-send-raw) ; term-bol
;;           ("C-b"     . term-send-left)
;;           ("C-f"     . term-send-right)
;;           ("C-p"     . previous-line)                    ; default
;;           ("C-n"     . next-line)                        ; default
;;           ("C-s"     . isearch-forward)                  ; default
;;           ("C-r"     . isearch-backward)                 ; default
;;           ("C-m"     . term-send-return)                 ; default
;;           ("C-y"     . term-paste)                       ; default
;;           ("M-f"     . term-send-forward-word)           ; default
;;           ("M-b"     . term-send-backward-word)          ; default
;;           ("M-o"     . term-send-backspace)              ; default
;;           ("M-p"     . term-send-up)                     ; default
;;           ("M-n"     . term-send-down)                   ; default
;;           ;; ("M-M"     . term-send-forward-kill-word)   ; default
;;           ("M-d"     . term-send-forward-kill-word)
;;           ;; ("M-N"     . term-send-backward-kill-word)  ; default
;;           ("M-DEL"   . term-send-backward-kill-word)
;;           ("M-r"     . term-send-reverse-search-history) ; default
;;           ("M-,"     . term-send-raw)                    ; default
;;           ("M-."     . comint-dynamic-complete)))        ; default

;;   (setq multi-term-buffer-name "term"))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :config (ws-butler-global-mode 1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package company
  :init (global-company-mode)
  :config
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  ;; TODO consider fuzzy matching https://docs.cider.mx/cider/usage/code_completion.html#_fuzzy_candidate_matching
  )

(use-package hideshow
  :bind (("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all)
         ("M--" . hs-hide-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  ;; Add `json-mode' and `javascript-mode' to the list
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-c D") 'duplicate-line)

;; (use-package highlight-indent-guides
;;     :hook (python-mode . highlight-indent-guides-mode)
;;     :config
;;     (setq highlight-indent-guides-method 'character)
;;     (setq highlight-indent-guides-character 9615) ;; left-align vertical bar
;;     (setq highlight-indent-guides-auto-character-face-perc 20))

;; (use-package symbol-overlay)

(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol)))))
    (delete-indentation arg)
    (when comt
      (save-excursion
        (move-end-of-line 1)
        (insert " ")
        (insert comt)))))

(defun paredit-remove-newlines ()
  "Removes extras whitespace and newlines from the current point
   to the next parenthesis."
  (interactive)
  (let ((up-to (point))
        (from (re-search-forward "[])}]")))
    (backward-char)
    (while (> (point) up-to)
      (paredit-delete-indentation))))

(use-package paredit
  ;; TODO When killing a newline delete all whitespace until next character (maybe just bring in Smartparens kill command)
  :bind (("M-^" . paredit-delete-indentation)
         ("C-^" . paredit-remove-newlines) ;; basically clean up a multi-line sexp
         ("C-<return>" . paredit-close-parenthesis-and-newline))
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'slime-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode))


;; Like: sp-kill-sexp (to delete the whole symbol not just forward like C-M-k does)
(defun kill-symbol ()
  (interactive)
  (backward-sexp) ;; TODO instead of backward-sexp, need to go to beginning of current symbol or go nowhere if already there
  (kill-sexp))

(global-set-key (kbd "M-k") 'kill-symbol)

;; (setq lsp-keymap-prefix "C-l")
;; (use-package lsp-mode
;;   ;; :hook ((clojure-mode . lsp)
;;   ;;        (clojurec-mode . lsp)
;;   ;;        (clojurescript-mode . lsp)
;;   ;;        (c++-mode . lsp)
;;   ;;        ;; (python-mode . lsp)
;;   ;;        ;; (javascript-mode . lsp)
;;   ;;        ;; (java-mode . lsp)
;;   ;;        ;; (c++-mode . lsp)
;;   ;;        )
;;   ;; :commands lsp
;;   :config
;;   (setq lsp-modeline-code-actions-segments '(icon)
;;         lsp-modeline-diagnostics-enable nil
;;         lsp-enable-file-watchers nil
;;         lsp-enable-indentation nil
;;         lsp-enable-on-type-formatting nil
;;         ;; Optimiazations lsp-mode https://emacs-lsp.github.io/lsp-mode/page/performance/
;;         gc-cons-threshold 100000000
;;         read-process-output-max (* 1024 1024)
;;         lsp-completion-provider :capf))
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil
;;         lsp-ui-sideline-show-code-actions nil))
;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol
;;   :config
;;   (define-key lsp-command-map "i"
;;     (lambda ()
;;       (interactive)
;;       (setq current-prefix-arg '(4))
;;       (call-interactively 'lsp-ivy-workspace-symbol))))
;; (use-package company-lsp
;;   :commands company-lsp)

;; ;; NOTE modify like below to defer
;; ;; (use-package lsp-mode
;; ;;     :hook (XXX-mode . lsp-deferred)
;; ;;     :commands (lsp lsp-deferred))

;; (use-package org-tempo)
(define-key org-mode-map (kbd "M-n") 'org-todo)
;; (define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org"
                             "~/org/guitar.org"
                             "~/org/learn.org")
      org-log-done t
      org-enforce-todo-dependencies t
      org-archive-location "archive/%s_archive::")


(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(define-key org-mode-map (kbd "C-c C-x C-a") 'org-archive-done-tasks)

(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; (use-package rjsx-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;;   (setq-default js2-basic-indent 2
;;                 ;; js2-basic-offset 2 ;; may need to use js-indent-level. js2-basic-offset is just an alias
;;                 js2-auto-indent-p t
;;                 js2-cleanup-whitespace t
;;                 js2-enter-indents-newline t
;;                 js2-indent-on-enter-key t
;;                 js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

;;   (add-hook 'rjsx-mode-hook
;;             (lambda ()
;;               (flycheck-select-checker "javascript-eslint")
;;               (electric-pair-mode 1)))

;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; ;; Idk what this does
;; ;; (use-package tern
;; ;;    :init (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; ;;    :config
;; ;;      (use-package company-tern
;; ;;         :ensure t
;; ;;         :init (add-to-list 'company-backends 'company-tern)))

;; (use-package js2-refactor
;;   :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
;;   :config (js2r-add-keybindings-with-prefix "C-c ."))

;; ;; Not sure what this does
;; (provide 'init-javascript)

;; (use-package typescript-mode
;;   :mode (("\\.ts\\'" . typescript-mode)
;;          ("\\.tsx\\'" . typescript-mode)))

;; (use-package go-projectile
;;   :init)

;; (use-package go-mode
;;   :init
;;   :config
;;   (use-package go-errcheck
;;     :ensure t
;;     )
;;   (defun my-go-mode-hook ()
;;     ;; golang.org/x/tools/cmd/goimports
;;     (setq gofmt-command "goimports")
;;     ;; call gofmt before saving
;;     (add-hook 'before-save-hook 'gofmt-before-save)
;;     (add-to-list 'exec-path "~/Repos/go/bin")
;;     ;; Customize compile command to run go build
;;     (if (not (string-match "go" compile-command))
;;         (set (make-local-variable 'compile-command)
;;              "go build -v && go vet"))
;;     ;; This proved to be too slow in big projects:
;;     ;; && go test -short -coverprofile cover.out && go tool cover -func cover.out

;;     (local-set-key (kbd "C-c C-c") 'compile)
;;     (local-set-key (kbd "C-c C-g") 'go-goto-imports)
;;     (local-set-key (kbd "C-c C-k") 'godoc)
;;     ;; github.com/kisielk/errcheck
;;     (local-set-key (kbd "C-c C-e") 'go-errcheck)
;;     (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
;;     ;; Godef jump key binding
;;     ;; code.google.com/p/rog-go/exp/cmd/godef
;;     (local-set-key (kbd "M-\"") 'godef-jump)
;;     ;; use company-go in go-mode
;;     (set (make-local-variable 'company-backends) '(company-go))
;;     (company-mode)

;;     (setenv "GOROOT" (shell-command-to-string ". /etc/zshrc; echo -n $GOROOT"))
;;     (setenv "GOPATH" (shell-command-to-string ". /etc/zshrc; echo -n $GOPATH")))

;;   ;; Ensure all linting passes, then use 'go build' to compile, then test/vet
;;   (defun setup-go-mode-compile ()
;;     (if (not (string-match "go" compile-command))
;;         (set (make-local-variable 'compile-command)
;;              "gometalinter.v1 --deadline 10s && go build -v && go test -v && go vet")))

;;     ;; set helm-dash documentation
;;   (defun go-doc ()
;;     (interactive)
;;     (setq-local helm-dash-docsets '("Go")))

;;   (add-hook 'go-mode-hook 'company-mode)
;;   (add-hook 'go-mode-hook 'go-eldoc-setup)
;;   (add-hook 'go-mode-hook 'highlight-word-hook)
;;   (add-to-list 'load-path (concat (getenv "GOPATH")
;;                                   "/src/github.com/golang/lint/misc/emacs"))
;;   ;; (require 'golint)
;;   ;; (add-hook 'go-mode-hook 'my-go-mode-hook)
;;   ;; (add-hook 'go-mode-hook 'go-doc)
;;   ;; (add-hook 'go-mode-hook 'setup-go-mode-compile)

;;   (require 'go-guru)
;;   (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
;;   )

;; (eval-after-load 'go-mode
;;   '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

;; ;; Completion integration
;; (use-package company-go
;;   :after go
;;   :config
;;   (setq tab-width 4)

;;   :bind (:map go-mode-map
;;               ("M-." . godef-jump)))

;; ;; ElDoc integration
;; (use-package go-eldoc
;;   :config
;;   (add-hook 'go-mode-hook 'go-eldoc-setup))

;; ;; Linting
;; (use-package flycheck-gometalinter
;;   :config
;;   (progn
;;     (flycheck-gometalinter-setup))
;;     ;; skip linting for vendor dirs
;;     (setq flycheck-gometalinter-vendor t)
;;     ;; use in test files
;;     (setq flycheck-gometalinter-test t)
;;     ;; only use fast linters
;;     (setq flycheck-gometalinter-fast t)
;;     ;; explicitly disable 'gotype' linter
;;     (setq flycheck-gometalinter-disable-linters '("gotype")))

;; (use-package toml-mode)

;; (use-package rust-mode
;;   :hook (rust-mode . lsp)
;;   :config
;;   (add-hook 'rust-mode-hook
;;             (lambda ()
;;               (electric-pair-mode 1)))
;;   )

;; ;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode)
;;   :config
;;   ;; (define-key cargo-minor-mode-map (kbd "C-c C-c C-r") (lambda ()
;;   ;;                                                        (interactive)
;;   ;;                                                        (message "hey")))
;;   )

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package glsl-mode)

;; (use-package slime-company)

;; ;; TODO full frame repl
;; ;; TODO switch from repl back to code with C-c C-z
;; (use-package slime
;;   :config
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "sbcl")
;;   (setq slime-lisp-implementations '((sbcl ("sbcl")))
;;       slime-default-lisp 'sbclp
;;       slime-contribs '(slime-fancy))
;;   (slime-setup '(slime-fancy slime-company slime-cl-indent))
;;   (defun slime-connect-localhost-4005 ()
;;         (interactive)
;;         (slime-connect "localhost" "4005"))
;;   (define-key slime-mode-map (kbd "C-c C-x j j") 'slime-connect-localhost-4005)
;;   (define-key slime-mode-map (kbd "C-c C-e") 'slime-eval-last-expression))

(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/home/benwiz/bin/")
(use-package clojure-snippets)
(use-package flycheck-clj-kondo) ;; NOTE .clj-kondo/ is in .emacs.d/ for version control, it must be symlinked to home `ln -s ~/.emacs.d/.clj-kondo/ ~/.clj-kondo/`

;; (use-package clj-refactor
;;   :init (add-hook 'clojure-mode-hook (lambda ()
;;     (yas-minor-mode 1)
;;     (clj-refactor-mode 1)
;;     (cljr-add-keybindings-with-prefix "C-c C-m"))))

;; (use-package anakondo
;;   :ensure t
;;   :commands anakondo-minor-mode)

(defun insert-discard ()
  "Insert #_ at current location."
  (interactive)
  (insert "#_"))

(use-package clojure-mode
 :bind (("C-c d f" . cider-code)
        ("C-c d g" . cider-grimoire)
        ("C-c d w" . cidler-grimoire-web)
        ("C-c d c" . clojure-cheatsheet)
        ("C-c d d" . dash-at-point)
        ("C-c C-;" . insert-discard))
 :init
 (setq clojure-indent-style 'align-arguments
       clojure-align-forms-automatically t)
 :config
 (add-hook 'clojure-mode-hook 'paredit-mode)
 ;; (add-hook 'clojure-mode-hook #'anakondo-minor-mode)
 ;; (add-hook 'clojurescript-mode-hook #'anakondo-minor-mode)
 ;; (add-hook 'clojurec-mode-hook #'anakondo-minor-mode)
 (require 'flycheck-clj-kondo)
 )

(defun cider-send-and-evaluate-sexp ()
  "Sends the sexp located before the point or
the active region to the REPL and evaluates it.
Then the Clojure buffer is activated as if nothing happened."
  (interactive)
  (if (not (region-active-p))
      (cider-insert-last-sexp-in-repl)
    (cider-insert-in-repl
     (buffer-substring (region-beginning) (region-end)) nil))
  (cider-switch-to-repl-buffer)
  (cider-repl-closing-return)
  (cider-switch-to-last-clojure-buffer)
  (message ""))

(use-package cider
  :commands (cider cider-connect cider-jack-in cider-jack-in-clj cider-jack-in-cljs)

  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t
        ;; Stop error buffer from popping up while working in buffers other than the REPL:
        nrepl-popup-stacktraces nil)

  ;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-mode-hook 'company-mode)

  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'superword-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)

  :bind (:map cider-mode-map
         ("C-c C-v C-c" . cider-send-and-evaluate-sexp)
         ("C-c C-p"     . cider-pprint-eval-last-sexp-to-comment)
         ("C-c C-<tab>" . cider-format-edn-region))
        (:map cider-repl-mode-map
              ("C-c C-l"     . cider-repl-clear-buffer)
              ("C-c C-<tab>" . cider-format-edn-region))

  :config
  (setq exec-path (append exec-path '("/home/benwiz/.yarn/bin")))
  (setq exec-path (append exec-path '("/home/benwiz/bin")))
  ;; (setq exec-path (append '("/Users/benwiz/.nvm/versions/node/v12.16.1/bin") exec-path))
  (add-to-list 'exec-path "/home/benwiz/.nvm/versions/node/v14.4.0/bin")
  (setq exec-path (append '("/Users/benwiz/.yarn/bin") exec-path))
  (setq cider-cljs-repl-types '((nashorn "(do (require 'cljs.repl.nashorn) (cider.piggieback/cljs-repl (cljs.repl.nashorn/repl-env)))" cider-check-nashorn-requirements)
                              (figwheel "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))" cider-check-figwheel-requirements)
                              (figwheel-main cider-figwheel-main-init-form cider-check-figwheel-main-requirements)
                              (figwheel-connected "(figwheel-sidecar.repl-api/cljs-repl)" cider-check-figwheel-requirements)
                              (node "(do (require 'cljs.repl.node) (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))" cider-check-node-requirements)
                              (weasel "(do (require 'weasel.repl.websocket) (cider.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))" cider-check-weasel-requirements)
                              (boot "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))" cider-check-boot-requirements)
                              (app cider-shadow-cljs-init-form cider-check-shadow-cljs-requirements) ;; this is what is being added
                              (shadow cider-shadow-cljs-init-form cider-check-shadow-cljs-requirements)
                              (shadow-select cider-shadow-select-cljs-init-form cider-check-shadow-cljs-requirements)
                              (custom cider-custom-cljs-repl-init-form nil))))

(defun ha/cider-append-comment ()
  (when (null (nth 8 (syntax-ppss)))
    (insert " ; ")))

(advice-add 'cider-eval-print-last-sexp :before #'ha/cider-append-comment)
