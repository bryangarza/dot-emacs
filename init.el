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

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(load-file "~/.private.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-when-compile
  (require 'use-package))

(setq bryan-pkg-full
      '(bryan-themes
        bryan-general
        bryan-interface
        bryan-paredit
        bryan-hooks
        bryan-evil
        bryan-cider
        bryan-paren
        bryan-auto-complete
        bryan-frontend
        bryan-markdown
        bryan-json
        bryan-helm
        bryan-better-splits
        bryan-rename-or-delete-buffer-and-file
        bryan-minibuffer-keyboard-quit
        bryan-util
        bryan-flycheck
        bryan-c
        bryan-ocaml
        bryan-haskell
        bryan-circe
        bryan-helm-swoop
        bryan-racket
        bryan-multiple-cursors
        bryan-clean-mode-line
        bryan-toggle-split-or-rotate-windows
        elisp-slime-nav
        bryan-smart-quotes
        bryan-scala
        bryan-magit
        bryan-pandoc
        bryan-ace-jump
        bryan-org-present
        bryan-hy
        bryan-rcirc
        bryan-w3m
        bryan-rust
        bryan-elisp
        bryan-clojure
        bryan-keybindings
        ))

(dolist (file bryan-pkg-full)
  (require file))
