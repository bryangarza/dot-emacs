;; c.el

(defun c-mode-custom-hook ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (bind-key "C-c C-c" 'recompile c-mode-map))

(add-hook 'c-mode-common-hook 'paredit-nonlisp-hook)
(add-hook 'c-mode-common-hook 'c-mode-custom-hook)

(provide 'bryan-c)
