;; TODO revisit the following, probably not needed anymore
;; Try to speed up font lock, I have identified it using the profiler as the major bottleneck in my lag
(setq jit-lock-stealth-time 16
      ;; jit-lock-defer-time nil
      ;; jit-lock-defer-contextually t ;; this is something to look into, I copied the `t` from the wiki but it isn't the default
      ;; jit-lock-stealth-nice 0.5
      )
(setq-default font-lock-multiline 'undecided)

;; Underline/highlight selected line
(global-hl-line-mode 1)

;; Light theme. I like the default theme more than any other light theme I found.
;; The following are global customizations I intend to apply to the default theme. There could be a more constrained way which would be better.
;; TODO need to set this following gray font, it is too light currently (make sure to be in light theme when reading this)
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 135)
(set-face-attribute 'hl-line nil :background "#e3ffe3")
(set-face-attribute 'region nil :background "#E4E4E4")
(set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-constant-face nil :foreground "#255814") ;; Forest Green is default; DarkGreen is good; hex is darker forest green
(set-face-attribute 'font-lock-type-face nil :foreground "#006060") ;; DarkCyan (#008b8b) is default, hex is darker version
(set-face-attribute 'trailing-whitespace nil :background "#e0eeff")
;; TODO better isearch colors for light theme

(setq custom-theme-directory "~/.emacs.d/themes")

;; Dark theme
(defun load-spolsky ()
  "Load Spolsky."
  (load-theme 'spolsky t)
  (let ((custom--inhibit-theme-enable nil)) ;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27
    (custom-theme-set-faces
     'spolsky
     `(default ((t (:foreground "#F2F2F2" :background "#161A1F"))))
     `(hl-line ((t (:background "#222e3a" :underline nil :overline nil)))) ;; 7b96b1
     `(font-lock-comment-delimiter-face ((t (:foreground "#8C8C8C" :slant italic))))
     `(font-lock-comment-face ((t (:foreground "#8C8C8C" :slant italic))))
     `(trailing-whitespace ((t (:background "#5a708c"))))
     `(lsp-face-highlight-textual ((t (:background "#353535")))) ;; "#323E30" ;; "#555" is same as selection color, the other one is half way between hl-line and trailing-whitespace
     `(org-level-4 ((t (:foreground "#EEEEBF"))))
     `(isearch ((t (:foreground "#222222" :background "#b5ff80")))) ;; selected isearch results TODO cursor picks up background color and becomes very ugly
     `(lazy-highlight ((t (:foreground "#222222" :background "#FF80F4")))) ;; other isearch results
     ))
  )

;; Start in spolsky
(add-hook 'after-make-frame-functions (lambda (frame) (load-spolsky)))
(load-spolsky)

;; Toggle themes
(defun toggle-theme ()
  "Toggle Spolsky theme on and off."
  (interactive)
  (if (member 'spolsky custom-enabled-themes)
      (progn
        (f-write-text "light" 'utf-8 "~/.theme")
        (disable-theme 'spolsky))
    (progn
      (f-write-text "dark" 'utf-8 "~/.theme")
      (load-spolsky) ;; (enable-theme 'spolsky) ;; This should work (and did work I think) because spolsky has been loaded before
      )))
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10>") 'toggle-theme)

;; Modeline
(use-package nerd-icons) ;; NOTE Must run `M-x nerd-icons-install-fonts` to install icons
(use-package doom-modeline
  ;; https://github.com/seagle0128/doom-modeline#customize
  :hook (window-setup . doom-modeline-mode) ;; (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 20)
  ;; (setq doom-modeline-persp-name t)
  ;; (setq doom-modeline-display-default-persp-name t)
  (setq doom-modeline-env-version t)
  )
