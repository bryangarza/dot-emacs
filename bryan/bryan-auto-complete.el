;; auto-complete.el

(use-package auto-complete
  :ensure t
  :defer t
  :config
  (progn
    (require 'auto-complete-config)
    (auto-complete-mode t)))

(provide 'bryan-auto-complete)
