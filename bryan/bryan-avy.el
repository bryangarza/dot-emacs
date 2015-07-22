;; avy.el

(use-package avy
  :ensure t
  :config
  (progn
    (avy-setup-default)

    ;; Input one char, jump to it with a tree.
    (global-set-key (kbd "C-:") 'avy-goto-char)

    ;; Input two consecutive chars, jump to the first one with a tree.
    (global-set-key (kbd "C-'") 'avy-goto-char-2)

    ;; Input zero chars, jump to a line start with a tree.
    (global-set-key (kbd "M-g f") 'avy-goto-line)

    ;; Input one char at word start, jump to a word start with a tree.
    (global-set-key (kbd "M-g w") 'avy-goto-word-1)

    ;; Input zero chars, jump to a word start with a tree.
    (global-set-key (kbd "M-g e") 'avy-goto-word-0)

    (define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
    (define-key evil-motion-state-map (kbd "P") #'avy-goto-line))
  )

(provide 'bryan-avy)
