(if (file-exists-p "~/.emacs.d/emacs.env")
    (use-package load-env-vars
      :init
      (load-env-vars "~/.emacs.d/emacs.env")))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
