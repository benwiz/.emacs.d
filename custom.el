(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "e6e8f3ea4b626a0c89ff05f17d65485789e41aca7c569cc717362ada09530a4f" "a68e09ae8b9b1e4f0e622f9620791c00380350960200dd5933f84fec83aedcc5" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "ea73dbef6fcf27dec442ecdad4328d1e66fc5f29ddc72c824cf8e4a92d3442d2" "a2015ed140a9979715d451046651962886b8c15b2bea969da13dc935049c76d5" default))
 '(jabber-activity-count-in-title nil)
 '(jabber-activity-make-strings 'jabber-activity-make-strings-shorten)
 '(jabber-activity-mode t)
 '(jabber-activity-shorten-minimum 1)
 '(jabber-mode-line-compact t)
 '(jabber-mode-line-mode nil)
 '(linum-format " %7i ")
 '(package-selected-packages
   '(paradox org-tempo switch-buffer-functions free-keys multiple-cursors counsel-projectile all-the-icons symbol-overlay ivy-filthy-rich ivy-rich anakondo typescript-mode yaml-mode redtick tomatinho lsp-ui company-lsp lsp-ivy projectile doom-modeline glsl-mode dictionary define-word LilyPond-mode lilypond-mode lilypond slime-company magit magit-margin restclient flycheck-gometalinter company-go go-errcheck go-projectile multi-term exwm dashboard page-break-lines solarized-theme highlight elfeedn emojify restart-emacs mew jabberx-autoloads jabber-autoloads jabber hangups hackernews pomodoro editorconfig exec-path-from-shell rjsx-mode js2-refactor company-tern tern js2-mode flycheck-rust cargo rust-mode toml-mode lsp-mode markdown-mode itail misc auto-highlight-symbol highlight-symbol avy ws-butler wttrin git-link scratch dired-x dired undo-tree clj-refactor clojure-snippets auto-package-update wgrep highlight-indent-guides htmlize git-gutter diff-hl fic-mode expand-region rainbow-blocks dumb-jump load-env-vars company company-mode slamhound counsel swiper ivy rainbow-delimiters gnu-elpa-keyring-update queue-0\.2 ob-clojure cider flycheck-clj-kondo paredit flycheck use-package))
 '(safe-local-variable-values
   '((eval progn
           (setenv "GOOGLE_APPLICATION_CREDENTIALS" "/app/certs/breezeehr.com breeze-ehr-b8d518d1a8b6.json"))
     (eval add-hook 'cider-connected-hook
           (lambda nil
             (shell-command "./git-version.sh"))
           nil t)
     (eval progn
           (setenv "breeze.ehr.services.config-url" "file:dev-resources/prod-service-config.edn")
           (setenv "GOOGLE_APPLICATION_CREDENTIALS" "/home/benwiz/code/master-at-arms/mast/dev-resources/mast-service-account.json")
           (setenv "breeze.ehr.services.account-service-key" "accounts")
           (setenv "breeze.ehr.session.cookie-attrs.secure" "false")
           (setenv "breeze.ehr.session.cookie-name" "mast-ec")
           (setenv "breeze.ehr.session.store.impl" "encrypted-cookie")
           (setenv "breeze.ehr.session.store.encrypted-cookie.key" "00000000000000000000000000000000")
           (setenv "breeze.ehr.goog.app-name" "mast")
           (setenv "breeze.ehr.goog.service-account-id" "mast-service@breeze-ehr.breezeehr.com.iam.gserviceaccount.com")
           (setenv "breeze.ehr.goog.p12-file" "dev-resources/Breeze-EHR_local-development_a3f4668ebdc0.p12")
           (setenv "breeze.ehr.goog.do-document-uploads" "true")
           (setenv "breeze.ehr.rule-engine.do-receive" "true")
           (setenv "breeze.ehr.http.do-send-stacktrace" "true")
           (setenv "breeze.ehr.use-shared-hitch-graph" "true"))
     (cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-clojure-cli-global-options . "-A:fig")
     (cider-clojure-cli-global-options . "-A:dev:runtime")
     (cider-clojure-cli-global-options . "-A:emacs")
     (cider-clojure-cli-global-options . "-A:emacs:test")
     (cider-preferred-build-tool . clojure-cli)
     (eval progn
           (setenv "breeze.ehr.services.config-url" "file:dev-resources/prod-service-config.edn")
           (setenv "breeze.ehr.services.account-service-key" "accounts")
           (setenv "breeze.ehr.session.cookie-attrs.secure" "false")
           (setenv "breeze.ehr.session.cookie-name" "mast-ec")
           (setenv "breeze.ehr.session.store.impl" "encrypted-cookie")
           (setenv "breeze.ehr.session.store.encrypted-cookie.key" "00000000000000000000000000000000")
           (setenv "breeze.ehr.goog.app-name" "mast")
           (setenv "breeze.ehr.goog.service-account-id" "local-development@breeze-ehr.breezeehr.com.iam.gserviceaccount.com")
           (setenv "breeze.ehr.goog.p12-file" "dev-resources/Breeze-EHR_local-development_a3f4668ebdc0.p12")
           (setenv "breeze.ehr.goog.do-document-uploads" "true")
           (setenv "breeze.ehr.rule-engine.do-receive" "true")
           (setenv "breeze.ehr.http.do-send-stacktrace" "true")
           (setenv "breeze.ehr.use-shared-hitch-graph" "true"))
     (cider-shadow-default-options . ":app")
     (cider-default-cljs-repl . shadow)
     (cider-preferred-build-tool . shadow-cljs)
     (setenv "breeze.ehr.use-shared-hitch-graph" "true"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-activity-personal-face ((t (:foreground "cyan" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "cyan" :weight bold)))))
