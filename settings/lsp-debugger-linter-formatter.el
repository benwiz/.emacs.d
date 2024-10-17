;; Always use company for completion

(use-package company
  :bind (("C-<return>" . company-complete-common-or-cycle))
  :hook (prog-mode . company-mode))

(define-key prog-mode-map (kbd "<tab>") #'indent-for-tab-command)

;; Pick one of these two options:
;; (1) eglot augmented with flymake and reformatter. use dape for debugger (I have yet to get dape to work)
;; (2) lsp-mode for everything

;;;;;;;;;;;
;; EGLOT ;;
;;;;;;;;;;;

(use-package eglot
  :bind (("C-c C-a" . eglot-code-actions))
  ;; :hook (python-base-mode . eglot-ensure) ;; let pet-mode turn on eglot
  ;; :after pet
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
                 ("pyright-langserver" "--stdio")
                 ;; ,(eglot-alternatives ;; set priority
                 ;;   '(("pyright-langserver" "--stdio")
                 ;;     "pylsp"
                 ;;     "pyls"
                 ;;     ("poetry" "run" "pyright-langserver" "--stdio")
                 ;;     "jedi-language-server"))
                 ))
  )

;; ;; Using `pet-mode` is a crutch. I should handle this in .dir-locals.el
(use-package pet
  :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-base-mode-hook
            (lambda ()
              ;; (setenv "VIRTUAL_ENV" "/Users/j/lennar/mimir/app/.emacsvenv")
              (setq-local python-shell-virtualenv-root (pet-virtualenv-root))
              (setq-local python-shell-interpreter (pet-executable-find "python"))
              (pet-eglot-setup)
              (eglot-ensure)
              ))
  )

(use-package flymake-ruff
  :hook ((eglot-managed-mode . flymake-ruff-load)
         ((python-mode python-ts-mode) . flymake-mode)
         ((python-mode python-ts-mode) . flymake-ruff-load)))

(use-package reformatter
  :hook
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format ;; note this creates ruff-format-buffer, ruff-format-region, ruff-format-on-save-mode
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define ruff-check-fix ;; this is definitely a hack shoving it into reformatter but it's kind of convenient
    :program "ruff"
    :args `("check" "--fix" "--stdin-filename" ,buffer-file-name "-")))

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a"))


;;   ;; :hook
;;   ;; ;; Save breakpoints on quit
;;   ;; ((kill-emacs . dape-breakpoint-save)
;;   ;;  ;; Load breakpoints on startup
;;   ;;  (after-init . dape-breakpoint-load))

;;   ;; :init
;;   ;; To use window configuration like gud (gdb-mi)
;;   ;; (setq dape-buffer-window-arrangement 'gud)

;;   :config
;;   ;; Info buffers to the right
;;   ;; (setq dape-buffer-window-arrangement 'right)

;;   ;; Global bindings for setting breakpoints with mouse
;;   (dape-breakpoint-global-mode)

;;   ;; Pulse source line (performance hit)
;;   ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

;;   ;; To not display info and/or buffers on startup
;;   ;; (remove-hook 'dape-start-hook 'dape-info)
;;   ;; (remove-hook 'dape-start-hook 'dape-repl)

;;   ;; To display info and/or repl buffers on stopped
;;   ;; (add-hook 'dape-stopped-hook 'dape-info)
;;   ;; (add-hook 'dape-stopped-hook 'dape-repl)

;;   ;; Kill compile buffer on build success
;;   ;; (add-hook 'dape-compile-hook 'kill-buffer)

;;   ;; Save buffers on startup, useful for interpreted languages
;;   ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

;;   ;; Projectile users
;;   ;; (setq dape-cwd-fn 'projectile-project-root)
;;   )



;;;;;;;;;;;;;;
;; LSP MODE ;;
;;;;;;;;;;;;;;


;; (use-package lsp-mode
;;   :commands lsp lsp-deferred
;;   ;; :hook ((python-mode . lsp-deferred)
;;   ;;        (python-ts-mode . lsp-deferred))
;;   :init
;;   (setq lsp-keymap-prefix "C-c l"
;;         lsp-headerline-breadcrumb-enable nil
;;         lsp-modeline-diagnostics-enable nil
;;         )
;;   )

;; (use-package lsp-docker
;;   :init
;;   (defvar lsp-docker-client-packages
;;     '(
;;       ;; lsp-pylsp
;;       ;; lsp-css lsp-clients lsp-bash lsp-go lsp-html lsp-typescript
;;       ;; lsp-terraform lsp-clangd
;;       ))
;;   (setq lsp-docker-client-configs
;;         '(
;;           ;; (:server-id pylsp :docker-server-id mimir-mimir-1 :server-command "pylsp")
;;           ))
;;   :config
;;   (lsp-docker-init-clients
;;    :path-mappings '(("/Users/j/lennar/mimir/app" . "/app"))
;;    :client-packages lsp-docker-client-packages
;;    :client-configs lsp-docker-client-configs)2
;;   )

;; (defvar lsp-docker-client-packages
;;     '(lsp-pylsp
;;       ;; lsp-css lsp-clients lsp-bash lsp-go lsp-html lsp-typescript
;;       ;; lsp-terraform lsp-clangd
;;                     ))

;; (setq lsp-docker-client-configs
;;       '(
;;       ;; (:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
;;       ;; (:server-id clangd :docker-server-id clangd-docker :server-command "clangd")
;;       ;; (:server-id css-ls :docker-server-id cssls-docker :server-command "css-languageserver --stdio")
;;       ;; (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
;;       ;; (:server-id gopls :docker-server-id gopls-docker :server-command "gopls")
;;       ;; (:server-id html-ls :docker-server-id htmls-docker :server-command "html-languageserver --stdio")
;;       (:server-id pylsp :docker-server-id pyls-docker :server-command "pylsp")
;;       ;; (:server-id pyright :docker-server-id mimir :server-command "pyright-languageserver --stdio")
;;       ;; (:server-id ts-ls :docker-server-id tsls-docker :server-command "typescript-language-server --stdio")
;;       ))


;; (use-package flycheck)

;; (use-package lsp-ivy
;;   :bind (("C-c l g s" . lsp-ivy-workspace-symbol))
;;   :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-pyright
;;   :hook ((python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           ;; (lsp)
;;                           ))
;;          (python-ts-mode . (lambda ()
;;                              (require 'lsp-pyright)
;;                              ;; (lsp)
;;                              )))
;;   )

;; (use-package with-venv)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :commands dap-debug
;;   :hook ((python-mode . dap-ui-mode)
;;          (python-mode . dap-mode)
;;          (python-ts-mode . dap-ui-mode)
;;          (python-ts-mode . dap-mode))
;;   :config
;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   ;; (defun dap-python--pyenv-executable-find (command)
;;   ;;   (with-venv (executable-find "python")))
;;   ;; (add-hook 'dap-stopped-hook
;;   ;;           (lambda (arg) (call-interactively #'dap-hydra)))
;;   )
