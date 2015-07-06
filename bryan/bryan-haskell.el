;; haskell.el

;; (add-to-list 'load-path "~/.emacs.d/lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/lisp/haskell-mode/")

(use-package hi2
  :ensure t)

(defun haskell-custom-hook ()
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; Getting tired of these 2 sometimes
  ;; (flycheck-mode)
  ;; (paredit-mode 1)
  (turn-on-hi2)
  (electric-indent-mode nil)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (evil-leader/set-key
    "SPC" 'haskell-mode-contextual-space
    "s" 'haskell-mode-tag-find)
  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; ;; Interactively choose the Cabal command to run.
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

  (custom-set-variables
   '(haskell-process-type 'cabal-repl)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)))


(use-package haskell-mode
  :ensure t
  :config
  (progn
    (bind-key "C-c C-c" 'haskell-compile haskell-mode-map)
    (bind-key "C-c C-c" 'haskell-compile haskell-mode-map)))

(add-hook 'haskell-mode-hook 'haskell-custom-hook)

(provide 'bryan-haskell)
