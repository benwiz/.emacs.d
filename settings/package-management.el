(require 'package)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Fetch pacakges when package-archive-contents does not exist
(when (not package-archive-contents) (package-refresh-contents))

;; List packages to install
(setq package-list '(use-package))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Suggest to upgrade packages occasionally, TODO would be better to ask the first time emacs is opened each month
;; TODO I never see this because of emacsclient. So I've shut it off for now.
;; (when (eq 0 (random 50))
;;   (when (y-or-n-p-with-timeout "Do you want to check packages for upgrades? " 6 nil)
;;     (package-list-packages)
;;     (message "Run `Shift-U x` to upgrade")))

;; ;; force packages to always be installed, no need to defer with emacsclient, I think
;; NOTE must set `:ensure nil` if not a package.el package, like dired
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; To upgrade manually `M-x list-packages U x`, if that is not convenient check below stackoverflow post for some helper functions
;; https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
;; Alternatively use auto-package-update but that caused surprise issues in the past when upgrading blindly

(use-package gnu-elpa-keyring-update)

(require 'misc)
