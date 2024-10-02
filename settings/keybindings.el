(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "C-i"))
(global-unset-key (kbd "C-x C-x"))
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-x C-u"))

(defun my-kill-this-buffer ()
  "Kill current buffer. Built in kill-this-buffer is meant to be used from GUI menu bar, according to some reddit post."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my-kill-this-buffer) ;; Don't ask which buffer, just do it
(global-set-key (kbd "C-x C-x") 'mode-line-other-buffer)
(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c n") 'narrow-to-defun)
(global-set-key (kbd "C-c w") 'widen)
(global-set-key (kbd "C-l") (lambda ()
                              (interactive)
                              (recenter-top-bottom (round (* 0.37 (window-height))))))
