;; pandoc.el

(use-package pandoc-mode
  :ensure t)

(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(provide 'bryan-pandoc)
