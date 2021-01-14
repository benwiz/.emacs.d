;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(htmlize yaml-mode ws-butler use-package undo-tree switch-buffer-functions scratch restclient restart-emacs redtick pomodoro paradox markdown-mode magit load-env-vars gnu-elpa-keyring-update git-link flycheck-clj-kondo fic-mode exwm expand-region exec-path-from-shell editorconfig doom-modeline diff-hl dictionary counsel-projectile company clojure-snippets clj-refactor auto-package-update))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'org-html-export-to-html t t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
