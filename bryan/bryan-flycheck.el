;; flycheck.el

(use-package flycheck
  :ensure t
  :defer t
  :init (setq-default flycheck-disabled-checkers '(javascript-jshint)))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'bryan-flycheck)
