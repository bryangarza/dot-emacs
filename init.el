(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/bryan")

(require 'bryan-packages)
(unless (bryan/packages-installed-p)
  (message "%s" "Installing required packages...")
  (package-refresh-contents)
  (dolist (pkg bryan/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(add-to-list 'load-path "~/.emacs.d/customizations")

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(load-file "~/.private.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-when-compile
  (require 'use-package))

(require 'org)
(require 'bryan-themes)
(require 'bryan-general)
(require 'bryan-interface)
(require 'bryan-paredit)
(require 'bryan-hooks)
(require 'bind-key)
(require 'bryan-evil)
(require 'bryan-cider)
(require 'bryan-paren)
(require 'bryan-auto-complete)
(require 'bryan-frontend)
(require 'bryan-markdown)
(require 'bryan-json)
(require 'bryan-helm)
(require 'bryan-better-splits)
(require 'bryan-rename-or-delete-buffer-and-file)
(require 'bryan-minibuffer-keyboard-quit)
(require 'bryan-util)
(require 'bryan-flycheck)
(require 'bryan-c)
(require 'bryan-ocaml)
(require 'bryan-haskell)
(require 'bryan-circe)
(require 'bryan-helm-swoop)

(setq geiser-active-implementations '(racket))

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(use-package auto-complete
  :config (add-to-list 'ac-modes 'geiser-repl-mode))

(require 'bryan-multiple-cursors)
(require 'bryan-clean-mode-line)
(require 'bryan-toggle-split-or-rotate-windows)
(require 'elisp-slime-nav)
(require 'bryan-smart-quotes)
(require 'bryan-scala)
(require 'bryan-magit)
(require 'bryan-pandoc)
(require 'bryan-ace-jump)
(require 'bryan-org-present)
(require 'bryan-hy)
(require 'bryan-rcirc)
(require 'bryan-w3m)
(require 'tramp)
(require 'bryan-rust)
(require 'bryan-keybindings)

(load "elisp-editing.el")
(load "setup-clojure.el")
