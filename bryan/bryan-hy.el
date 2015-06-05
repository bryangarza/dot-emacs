;; hy.el

(use-package hy-mode
  :ensure t)

(add-hook 'hy-mode-hook 'paredit-mode)

(provide 'bryan-hy)
