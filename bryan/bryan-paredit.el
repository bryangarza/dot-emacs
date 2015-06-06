;; paredit.el

(use-package paredit
  :ensure t
  :config
  (progn

    (add-hook 'paredit-mode-hook 'evil-paredit-mode)

    (defun paredit-nonlisp-hook ()
      "Turn on paredit mode for non-lisps."
      (interactive)
      (set (make-local-variable 'paredit-space-for-delimiter-predicates)
           '((lambda (endp delimiter) nil)))
      (paredit-mode 1))

    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))))

(provide 'bryan-paredit)
