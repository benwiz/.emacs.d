(defun load-init-el ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i") 'load-init-el)
