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

(require 'org)
(setq org-src-fontify-natively t)

(require 'bryan-themes)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(desktop-save-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq electric-indent-mode                 t
      redisplay-dont-pause                 t
      apropos-do-all                       t
      require-final-newline                t
      save-interprogram-paste-before-kill  t
      mouse-yank-at-point                  t
      system-uses-terminfo                 nil
      ring-bell-function                   'ignore
      backup-directory-alist '(("." . "~/.emacs-backups"))
      scheme-program-name    "/usr/local/bin/mit-scheme"
      custom-file            "~/.emacs.d/custom.el")

;; (add-hook 'prog-mode-hook 'linum-mode)

(load custom-file)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)
;; (set-face-attribute 'default nil
;;                     :family "Droid Sans Mono Slashed" :height 140 :weight 'normal)
(set-face-attribute 'default nil
                    :family "Monaco" :height 150 :weight 'normal)
;; ლ(ಠ益ಠ)ლ ¡porque!
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode -1)
(column-number-mode t)
(global-hl-line-mode -1)
(blink-cursor-mode +1)
(display-time)
(display-battery-mode)

(add-hook 'paredit-mode-hook 'evil-paredit-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(use-package winner
  :ensure t
  :defer t
  :init (winner-mode 1))

(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")))

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config (setq evil-move-cursor-back nil))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

;; (use-package linum-relative
;;   :ensure t
;;   :init
;;   (progn
;;     (global-linum-mode 1)
;;     (setq linum-relative-current-symbol "")))

(load "elisp-editing.el")
(load "setup-clojure.el")

(require 'bryan-paren)
(require 'bryan-cider)

(use-package auto-complete
  :ensure t
  :defer t
  :config
  (progn
    (require 'auto-complete-config)
    (auto-complete-mode t)))

(require 'bryan-frontend)
(require 'bryan-markdown)
(require 'bryan-json)
(require 'bryan-helm)
(require 'bryan-better-splits)
(require 'bryan-rename-or-delete-buffer-and-file)
(require 'bryan-minibuffer-keyboard-quit)
(require 'bryan-keybindings)
(require 'bryan-util)

;; getting errors (when scheme file was opened)
(semantic-mode 0)

(use-package flycheck
  :ensure t
  :defer t
  :init (setq-default flycheck-disabled-checkers '(javascript-jshint)))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun paredit-nonlisp-hook ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(defun c-mode-custom-hook ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (bind-key "C-c C-c" 'recompile c-mode-map))

(add-hook 'js-mode-hook 'paredit-nonlisp-hook)
(add-hook 'c-mode-common-hook 'paredit-nonlisp-hook)
(add-hook 'c-mode-common-hook 'c-mode-custom-hook)

(require 'bryan-ocaml)
(require 'bryan-haskell)

(bind-key "C-x a r" 'align-regexp)

(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saved-places")))

;; IRC auth
(load-file "~/.private.el")
(require 'bryan-circe)
(require 'bryan-helm-swoop)

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))

(setq geiser-active-implementations '(racket))

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

(use-package auto-complete
  :config (add-to-list 'ac-modes 'geiser-repl-mode))

(require 'bryan-multiple-cursors)

(require 'bryan-clean-mode-line)

(add-hook 'window-startup-hook 'toggle-frame-maximized)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(bind-key "M-)" 'paredit-wrap-round-from-behind evil-motion-state-map)
(bind-key "M-)" 'paredit-wrap-round-from-behind evil-insert-state-map)

(require 'bryan-toggle-split-or-rotate-windows)
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode)
  (evil-leader/set-key
   "s" 'elisp-slime-nav-find-elisp-thing-at-point
   "S" 'pop-tag-mark
   "d" 'elisp-slime-nav-describe-elisp-thing-at-point))

;; Might be responsible for the scroll lag... maybe it was clashing w Evil?
;; (require 'smooth-scrolling)
;; (setq smooth-scroll-margin 5)

(setq doc-view-continuous t)

(require 'bryan-smart-quotes)

(require 'bryan-scala)

;; Replace default expand command
(global-set-key (kbd "M-/") 'hippie-expand)

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'bryan-pandoc)
(require 'bryan-ace-jump)
(require 'bryan-org-present)
(require 'bryan-hy)
(require 'bryan-rcirc)
(require 'bryan-w3m)
(require 'tramp)
;; C-x C-f /sudo::/path/to/file

(use-package rust-mode
  :ensure t)

(use-package evil-anzu
  :ensure t
  :config
  (progn
    (with-eval-after-load 'evil
      (require 'evil-anzu))))
