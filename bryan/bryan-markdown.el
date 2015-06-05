;; markdown.el

(use-package markdown-mode
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

(add-hook 'markdown-mode-hook 'pandoc-mode)

(provide 'bryan-markdown)
