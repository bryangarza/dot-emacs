;; frontend.el

(use-package web-mode
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    ;; (setq web-mode-enable-current-element-highlight t)
    ;; (setq web-mode-enable-current-column-highlight t)
    )
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.scss\\'"  . web-mode)))

(use-package sws-mode
  :mode "\\.styl$")

(use-package jade-mode
  :mode "\\.jade$")

(add-hook 'js-mode-hook 'paredit-nonlisp-hook)

(provide 'bryan-frontend)
