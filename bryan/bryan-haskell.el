;; haskell.el

;; (add-to-list 'load-path "~/.emacs.d/lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/lisp/haskell-mode/")

(use-package hi2
  :ensure t)

(defun haskell-custom-hook ()
  ;; Getting tired of these 2 sometimes
  ;; (flycheck-mode)
  ;; (paredit-mode 1)
  (turn-on-hi2)
  (inf-haskell-mode)
  (electric-indent-mode nil))

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (bind-key "C-c C-c" 'haskell-compile haskell-mode-map)
    (bind-key "C-c C-c" 'haskell-compile haskell-mode-map)))

(add-hook 'haskell-mode-hook 'haskell-custom-hook)

(provide 'bryan-haskell)
