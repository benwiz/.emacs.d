(defconst *is-a-mac* (eq system-type 'darwin))
(when (string-equal system-type "darwin")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar me/erc-nick               nil        "The ERC nick to use.")
(defvar me/erc-password           nil        "The ERC password to use.")
(defvar me/erc-port               nil        "The ERC port to use.")
(defvar me/erc-server             nil        "The ERC server to use.")
(defvar me/font-family            "Courier"  "The font to use.")
(defvar me/font-size-default      110        "The font size to use for default text.")
(defvar me/font-size-header-line  120        "The font size to use for the header-line.")
(defvar me/font-size-mode-line    110        "The font size to use for the mode-line.")
(defvar me/font-size-small        100        "The font size to use for smaller text.")
(defvar me/font-size-title        140        "The font size to use for titles.")

(let ((secret.el (expand-file-name "secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))

(require 'package)

(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; (package-refresh-contents)

;; list the packages you want
(setq package-list
    '(
      use-package
     ))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package gnu-elpa-keyring-update)

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
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace t                       ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 column-number-mode t
 )
(cd "~/")                                         ; Move to the user directory
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

;(if (eq window-system 'ns)
;  (add-to-list 'default-frame-alist '(maximized .))
;  (add-to-list 'default-frame-alist '(fullscreen .)))
(set-frame-parameter nil 'fullscreen 'fullboth)

(add-hook 'focus-out-hook #'garbage-collect)

(add-to-list 'custom-theme-load-path "/home/benwiz/.emacs.d/themes")
(load-theme 'spolsky t) ;; https://github.com/owainlewis/emacs-color-themes/blob/master/themes/spolsky-theme.el
(custom-theme-set-faces 'spolsky
  `(hl-line ((t (:background, "#151515" :underline nil))))
  )
(global-hl-line-mode 1)
(modify-face 'trailing-whitespace nil "#5a708c")


(use-package all-the-icons)
(use-package doom-modeline ;; alternative is moody for a simpler option
  ;; NOTE Must run `M-x all-the-icons-install-fonts` to install icons
  ;; https://github.com/seagle0128/doom-modeline#customize
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon t) ;; as in, isEdited? state
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 20)
  ;; (setq doom-modeline-persp-name t)
  ;; (setq doom-modeline-display-default-persp-name t)
  (setq doom-modeline-env-version t)
  )

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x k") 'kill-this-buffer) ;; Don't ask which buffer, just do it
(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)

  ;; (defun zap-until-char ()
  ;;   (interactive)
  ;;   (set-mark-command)
  ;;   (jump-char-forward c)
  ;;   )
  ;; (global-set-key (kbd "M-Z") 'zap-until-char)

(use-package htmlize)
(use-package wgrep)

(use-package highlight-indent-guides
;; :hook (prog-mode . highlight-indent-guides-mode) ;; I commented this out because I just want to manually toggle this
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character 9615) ; left-align vertical bar
  (setq highlight-indent-guides-auto-character-face-perc 20))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(use-package free-keys
  :bind ("C-h C-k" . 'free-keys))

(use-package jump-char
  :bind (("C-r" . jump-char-forward)
         ("C-S-r" . jump-char-backward)))

(defun load-init-el ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i") 'load-init-el)

(use-package magit
   :config
   (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode)
  (progn
    (setq git-gutter:separator-sign " "
          git-gutter:lighter " GG"))
  :config
  (progn
    (set-face-background 'git-gutter:deleted "#990A1B")
    (set-face-foreground 'git-gutter:deleted "#990A1B")
    (set-face-background 'git-gutter:modified "#00736F")
    (set-face-foreground 'git-gutter:modified "#00736F")
    (set-face-background 'git-gutter:added "#546E00")
    (set-face-foreground 'git-gutter:added "#546E00"))
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x v r" . git-gutter:revert-hunk)))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

(use-package swiper
  :init
  (set-face-attribute 'isearch nil :background "#FF9F93")
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch))

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
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep))

(use-package load-env-vars
  :init
  (load-env-vars "~/.emacs.d/emacs.env"))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package rainbow-delimiters
  :config
  (require 'cl-lib)
  (require 'color)
  (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
        (cl-callf color-saturate-name (face-foreground face) 20)))
  (require 'paren) ; show-paren-mismatch is defined in paren.el
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
    :foreground 'unspecified
    :inherit 'show-paren-mismatch)

  :hook
  (prog-mode . rainbow-delimiters-mode)) ;; WARNING: Being so general may break something, but going to go with it anyway

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package company
  :init (global-company-mode))

;; (use-package color-identifiers-mode
;;   :init
;;   (add-hook 'clojure-mode-hook 'color-identifiers-mode))

(use-package fic-mode
  :init
  (defface fic-face
    '((((class color))
    (:foreground "orange" :weight bold))
    (t (:weight bold)))
    "Face to fontify FIXME/TODO words"
    :group 'fic-mode)
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
  (add-hook 'prog-mode-hook 'fic-mode))

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

(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

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
  )

;; Like: sp-kill-sexp (to delete the whole symbol not just forward like C-M-k does)
(defun kill-symbol ()
  (interactive)
  (backward-sexp) ;; TODO instead of backward-sexp, need to go to beginning of current symbol or go nowhere if already there
  (kill-sexp))
(global-set-key (kbd "M-k") 'kill-symbol)

;; Indent top level sexp
(defun indent-top-sexp ()
  (interactive)
  ;; TODO go to beginning or end of top level sexp
  ;; TODO select the whole sexp
  (indent-region)
  ;; TODO return to starting point
  )
;; (global-set-key (kbd "C-M-l") 'indent-top-sexp) ;; TODO use a different kbd



(use-package clojure-snippets)

(use-package flycheck-clj-kondo)

(use-package clojure-mode
 :bind (("C-c d f" . cider-code)
        ("C-c d g" . cider-grimoire)
        ("C-c d w" . cider-grimoire-web)
        ("C-c d c" . clojure-cheatsheet)
        ("C-c d d" . dash-at-point))
 :init
 (setq clojure-indent-style 'align-arguments
       clojure-align-forms-automatically t)
 ;; TODO figure out this indentation
 ;;(define-clojure-indent
 ;;  (:import 0)
 ;;  (:require 0))
 :config
 (require 'flycheck-clj-kondo))

(defun cider-send-and-evaluate-sexp ()
  "Sends the s-expression located before the point or the active
  region to the REPL and evaluates it. Then the Clojure buffer is
  activated as if nothing happened."
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
  :commands (cider cider-connect cider-jack-in)

  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        Cider-repl-history-size 1000
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
         ("C-c C-p"     . cider-eval-print-last-sexp)
         ("C-c M-o"     . cider-repl-clear-buffer)) ;; FIXME need to remove other binding

  :config
  (use-package slamhound)
  (setq exec-path (append exec-path '("/home/benwiz/.yarn/bin")))
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
