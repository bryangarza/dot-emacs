;; racket.el

(setq geiser-active-implementations '(racket))

(defun racket-mode-custom-hook ()
  (bind-key "s-e" 'geiser-eval-definition scheme-mode-map))

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-mode-hook 'racket-mode-custom-hook)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

(provide 'bryan-racket)
