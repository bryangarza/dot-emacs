;; magit.el

(use-package magit
  :ensure t
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-status-buffer-switch-function 'switch-to-buffer)))

(provide 'bryan-magit)
