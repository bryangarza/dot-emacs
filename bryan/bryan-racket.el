;; racket.el

(setq geiser-active-implementations '(racket))

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

(provide 'bryan-racket)
