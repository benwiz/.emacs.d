(add-to-list 'load-path "~/.emacs.d/settings")
(load "mac.el")
(load "custom.el")
(load "utils.el")
(load "defaults.el")
(load "package-management.el")
(load "env.el")
(load "theme.el")
(load "env.el")
(load "keybindings.el")

(use-package docker)

(use-package dired
  :ensure nil ;; dired is not package.el
  :bind (:map dired-mode-map
              ("<tab>" . dired-hide-subdir)
              ("C-i" . dired-kill-subdir))
  :config
  (setq dired-omit-files "^.~$")

  ;; dired - reuse current buffer by pressing 'a' instead of 'enter'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (require 'dired-x)
  (add-hook 'dired-mode-hook 'dired-omit-mode))

;; no lines in docview (actually i think it is off by default, the number is from something else) (pdf viewer)
;; (add-hook 'doc-view-minor-mode-hook (lambda () (linum-mode 0)))

(when (and (file-directory-p "~/code/bela-mode.el/")
           (file-directory-p "~/code/Bela/scripts/"))
  (use-package bela-mode
    :load-path "~/code/bela-mode.el"
    :init (setq bela-scripts-dir "~/code/Bela/scripts/")))

(defun jbw/git-commit-setup ()
  ;; (message (concat "AA 2:" (last (substring (shell-command-to-string "basename `git rev-parse --show-toplevel`") 0 -1))))
  ;; TODO limit to only master-at-arms2
  (insert
   (concat
    (nth 0 (split-string
            (shell-command-to-string
             "git diff --name-only --cached")
            "/"))
    ": ")))

(use-package magit
  ;; :init
  ;; (add-hook 'git-commit-setup-hook 'jbw/git-commit-setup)
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
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; persisting undo-tree history was really really slow, like absurdly slow, so stop persisting until I sort that out
  (setq undo-tree-auto-save-history nil))

(use-package ws-butler
    :hook (prog-mode . ws-butler-mode)
    :config (ws-butler-global-mode 1))

  (use-package expand-region
    :config
    (global-set-key (kbd "C-=") 'er/expand-region))

  (use-package company
    :init (global-company-mode)
    :config
    (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
    ;; TODO consider fuzzy matching https://docs.cider.mx/cider/usage/code_completion.html#_fuzzy_candidate_matching
    )

  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

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
       https://github.com/ShingoFukuyama/helm-swoop/blob/f67fa8a4fe3b968b7105f8264a96da61c948a6fd/helm-swoop.el#L657-668

  i.e. Move seamlessly from isearch to swiper search."
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
    )

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  ;; TODO may want to add ".gitignore" to this list
  (setq projectile-project-root-files (cons ".dir-locals.el" (cons ".projectile" projectile-project-root-files))
        projectile-project-root-files-functions #'(projectile-root-top-down
                                                   projectile-root-top-down-recurring
                                                   projectile-root-bottom-up

                                                   projectile-root-local))
  (projectile-mode 1))

(defun project-override (dir)
  (message (concat "dir " dir))
  ;; TODO dir-locals probably isn't the best solution, maybe should do dedicated .project
  (let ((override (or (locate-dominating-file dir "deps.edn")
                      (locate-dominating-file dir ".dir-locals.el"))))
    (message (concat "override " override))
    ;; TODO I think this output shape is broken? I've currently set up project-find-functions to use project-projectile as the primary search. Not ideal.
    (when override (cons 'vc (list override)))))

  ;; (project-projectile "~/master-at-arms2/jib/")
    ;; (project-try-vc "~/master-at-arms2/jib/")
    ;; (project-override "~/master-at-arms2/jib/")

;; TODO after project-switch-projects call project-switch-to-buffer
(use-package project
  :config
  (setq project-vc-merge-submodules nil)
  ;; (add-hook 'project-find-functions #'project-override)
  ;; important for not trying vc where there is no git file, the try vc should fail better
  ;; NOTE if project-projectile stops working, may need to re-install projectile above, but could do a minimally configged "off" version
  (setq project-find-functions '(project-projectile project-try-vc)
        ;; project-switch-commands #'project-switch-to-buffer ;; TODO rather, I'd like to show the 5 most recently opened buffers from this project
        ))

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

(use-package request)

;; TODO I'd like to show cider in the command line but eglot when inspected
;; (use-package eldoc
;;   :init
;;   )







(global-set-key (kbd "M-'") #'xref-find-references)
  ;; xref-after-jump-hook ;; this is not the hook I'm looking for
  ;;
  ;; pulse-momentary-highlight-region
  ;; xref-pulse-momentarily

 ;; set to non-nil to pulse. nil to hold until next keypress
(setq pulse-flag nil)
;; weird quirk, set pulse-flat to t and pulse-iterations to very high and can keep it highlighted while pressing keys
;; (setq pulse-iterations 10)

(use-package pet
  :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-base-mode-hook
            ;; 'pet-mode -10
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              ;; (pet-eglot-setup)
              ;; (eglot-ensure)
              ))
  )

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")


  ;; :hook
  ;; ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;;  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  ;; :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )

(defun eglot-code-actions' ()
  (interactive)
  (eglot-code-actions))

;; NOTE since upgrading to emacs29 I am trying out not using a hook to better understand my multiple server issue.
(use-package eglot
  :bind (("C-c C-a" . eglot-code-actions))
  :init
  ;; (add-hook 'prog-mode-hook #'eglot-ensure)
  (setq eglot-server-programs '(;; (clojure-mode . ("clojure-lsp")) ;; clojure-lsp is too memory heavy to always open
                                (python-mode . ("ruff" "server"))
                                (python-ts-mode . ("ruff" "server"))
                                ))
  )

;; (use-package flymake-ruff
;; :ensure t
;; :hook ((eglot-managed-mode . flymake-ruff-load)
;;        (python-mode . flymake-ruff-load)
;;        (python-ts-mode . flymake-ruff-load)))

(use-package lsp-mode
  :commands lsp lsp-deferred
  ;; :hook ((python-mode . lsp-deferred)
  ;;        (python-ts-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-diagnostics-enable nil
        )
  )

(use-package lsp-ivy
  :bind (("C-c l g s" . lsp-ivy-workspace-symbol))
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-pyright
  :ensure t
  :hook ((python-mode . (lambda ()
                           (require 'lsp-pyright)
                           ;; (lsp)
                           ))
         (python-ts-mode . (lambda ()
                             (require 'lsp-pyright)
                             ;; (lsp)
                             )))
  )

(use-package with-venv)

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
         (python-mode . dap-mode)
         (python-ts-mode . dap-ui-mode)
         (python-ts-mode . dap-mode))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  ;; (defun dap-python--pyenv-executable-find (command)
  ;;   (with-venv (executable-find "python")))
  ;; (add-hook 'dap-stopped-hook
  ;;           (lambda (arg) (call-interactively #'dap-hydra)))
  )

(use-package company
  :bind (("C-<return>" . company-complete-common-or-cycle))
  :hook (prog-mode . company-mode))

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   ;; (defalias 'browse-web #'eaf-open-browser)
;;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
;;   ;; unbind, see more in the Wiki
;;   )
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)

(define-key prog-mode-map (kbd "<tab>") #'indent-for-tab-command)

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
         ;; ("C-<return>" . paredit-close-parenthesis-and-newline)
         )
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

;; Note using poly-org because it overrides M-n and I can't figure out how to change that, it should be simple
  ;; not sure I like polymode, I may have preferred the old way, really I'd like both. This basically smashes the
  ;; former into this buffer.
(use-package poly-org)

(use-package org
  ;; :bind (("M-n" . org-todo)) ;; poly-org screwed this up, just learn the default `C-c C-t`
  :config
  ;; (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-agenda-files (list "~/org/work.org"
                               "~/org/school.org"
                               "~/org/guitar.org"
                               "~/org/learn.org")
        org-log-done t
        org-enforce-todo-dependencies t
        org-archive-location "archive/%s_archive::"
        org-startup-folded t))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(define-key org-mode-map (kbd "C-c C-x C-a") 'org-archive-done-tasks)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; make sure to `apt install pandoc`
  :init (setq markdown-command "pandoc --standalone --from gfm Form-Curator.md --highlight-style kate"))

(use-package markdown-toc)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-last-sexp)

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (setq-default js2-basic-indent 2
                ;; js2-basic-offset 2 ;; may need to use js-indent-level. js2-basic-offset is just an alias
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

  (add-hook 'rjsx-mode-hook
            (lambda ()
              (electric-pair-mode 1)))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; Idk what this does
;; (use-package tern
;;    :init (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;;    :config
;;      (use-package company-tern
;;         :ensure t
;;         :init (add-to-list 'company-backends 'company-tern)))

(use-package js2-refactor
  :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c ."))

;; Not sure what this does
(provide 'init-javascript)

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(add-hook 'json-mode-hook
          (lambda ()
          (make-local-variable 'js-indent-level)
            (setq tab-width 2)
          (setq js-indent-level 2)))

(use-package python-mode
  :bind (("C-c C-q" . python-shell-restart)
         )
  ;; :hook ((python-mode . eglot-ensure)
  ;;        (python-ts-mode . eglot-ensure))
  :init
  (setq tab-always-indent t)
  )

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
 ;; (add-hook 'clojure-mode-hook 'paredit-mode)
 ;; (add-hook 'clojure-mode-hook 'eglot-ensure)
 ;; (add-hook 'clojurescript-mode-hook 'eglot-ensure)
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

(defun ha/cider-append-comment ()
  (when (null (nth 8 (syntax-ppss)))
    (insert " ; ")))

(defun benwiz/cider-test-run-ns-tests ()
  (interactive)
  (cider-load-buffer)
  (cider-test-run-ns-tests nil))

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
        nrepl-popup-stacktraces nil ;; Stop error buffer from popping up while working in buffers other than the REPL:
        cider-jdk-src-paths '("/usr/lib/jvm/openjdk-17/source" "/usr/lib/jvm/openjdk-17/source/java.base")
        cider-enrich-classpath t
        )
  ;; (cider-add-to-alist 'cider-jack-in-dependencies "mx.cider/tools.deps.enrich-classpath" "1.18.5")

  (add-hook 'cider-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'superword-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-test-report-mode 'jcf-soft-wrap)

  :bind (:map cider-mode-map
              ("C-c C-v C-c" . cider-send-and-evaluate-sexp)
              ("C-c C-p"     . cider-pprint-eval-last-sexp-to-comment)
              ("C-c C-<tab>" . cider-format-edn-region)
              ("C-c C-t n"   . benwiz/cider-test-run-ns-tests)
              ;; ("<return>" . cider-repl-closing-return)
              )
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

(advice-add 'cider-eval-print-last-sexp :before #'ha/cider-append-comment)
