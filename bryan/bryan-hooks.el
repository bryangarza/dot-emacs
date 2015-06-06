;; hooks.el

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'window-startup-hook 'toggle-frame-maximized)

(provide 'bryan-hooks)
