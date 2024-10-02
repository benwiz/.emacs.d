(defconst *is-a-mac* (eq system-type 'darwin))
(when *is-a-mac*
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(setq mac-command-modifier 'meta)
