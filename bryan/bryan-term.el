;; term.el

(defun zshell ()
  "start a terminal with zshell"
  (interactive)
  (ansi-term "/usr/local/bin/zsh"))

(provide 'bryan-term)
