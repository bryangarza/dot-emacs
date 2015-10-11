(setq user-full-name "Bryan Garza"
      user-mail-address "bryanxmailing@gmail.com")

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(setq package-enable-at-startup nil)

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
      '(bryan-company
        bryan-themes
        bryan-org
        bryan-general
        bryan-interface
        bryan-hydra
        bryan-paredit
        bryan-hooks
        bryan-evil
        bryan-avy
        bryan-cider
        bryan-paren
        bryan-auto-complete
        bryan-frontend
        bryan-markdown
        bryan-json
        bryan-term
        bryan-helm
        bryan-better-splits
        bryan-rename-or-delete-buffer-and-file
        bryan-minibuffer-keyboard-quit
        bryan-util
        bryan-flycheck
        bryan-c
        bryan-ocaml
        bryan-haskell
        bryan-elm
        bryan-circe
        bryan-helm-swoop
        bryan-racket
        bryan-multiple-cursors
        bryan-clean-mode-line
        bryan-toggle-split-or-rotate-windows
        bryan-smart-quotes
        bryan-scala
        bryan-magit
        bryan-pandoc
        ;; bryan-ace-jump
        bryan-hy
        bryan-rcirc
        ;; bryan-w3m
        bryan-rust
        bryan-elisp
        bryan-clojure
        bryan-ox-html
        bryan-blog
        bryan-keybindings
        ))

(dolist (file bryan-pkg-full)
  (require file))

(global-set-key [s-mouse-1] 'browse-url-at-mouse)

(use-package ranger
  :ensure t
  :config
  (progn
    (setq evil-ranger-cleanup-eagerly t)
    (setq evil-ranger-ignored-extensions '("mkv" "iso" "mp4"))))

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-minibuffer)
    (setq max-mini-window-height 0.49)
    ;; (which-key-setup-side-window-right)
    ))

(use-package sqlup-mode
  :ensure t
  :config
  (progn
    ;; Capitalize keywords in SQL mode
    (add-hook 'sql-mode-hook 'sqlup-mode)
    ;; Capitalize keywords in an interactive session (e.g. psql)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    ;; Set a global keyword to use sqlup on a region
    ;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
    ))

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))
(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups )))

(use-package reveal-in-osx-finder
  ;; To load at the start up
  :ensure t
  ;; If you want to configure a keybinding (e.g., C-c z), add the following
  ;; :config
  ;; (progn
  ;;   (global-set-key (kbd "C-c z") 'reveal-in-osx-finder))
  )

(use-package erlang
  :ensure t)
;; is this doing anything? I don't know if I have this on somewhere already...
(setq doc-view-continuous t)
