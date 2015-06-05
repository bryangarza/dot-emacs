;; paren.el

(use-package paren
  :ensure t
  :init
  (progn
    (show-paren-mode 1)
    (setq show-paren-style 'parenthesis)
    ;; (set-face-background 'show-paren-match (face-background 'default))
    ;; (set-face-foreground 'show-paren-match "#def")
    ;; (set-face-attribute 'show-paren-match-face nil :weight 'medium :underline nil)
    (setq show-paren-delay 0)))

(provide 'bryan-paren)
