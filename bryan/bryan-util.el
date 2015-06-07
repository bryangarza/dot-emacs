;; util.el

(use-package winner
  :ensure t
  :defer t
  :config (winner-mode 1))

(use-package recentf
  :defer t
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-saved-items 200
          recentf-max-menu-items  25)))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))

(provide 'bryan-util)
