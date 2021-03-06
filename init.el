(setq user-full-name    "Bryan Garza"
      user-mail-address "brygarza@gmail.com")

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(setq package-enable-at-startup nil)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar bryan/packages
  '(use-package
    evil-paredit
    smartparens
    rainbow-delimiters
    rainbow-mode
    exec-path-from-shell
    debbugs
    geiser
    auctex)
  "Default packages")

(defun bryan/packages-installed-p ()
  (loop for pkg in bryan/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (bryan/packages-installed-p)
  (message "%s" "Installing required packages...")
  (package-refresh-contents)
  (dolist (pkg bryan/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/bryan/")

(defvar WORKSPACE_ROOT "~/workspace"
  "The directory where Brazil workspaces are located")

;;(load-file "~/.private.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-when-compile
  (require 'use-package))

(defun bryan/company ()
  (use-package company
    :ensure t
    :init
    (progn
      (add-hook 'after-init-hook #'global-company-mode)
      (setq company-tooltip-align-annotations t)
      (setq company-tooltip-limit 20)                      ; bigger popup window
      (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
      (setq company-echo-delay 0)                          ; remove annoying blinking
      (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
      )))

(defun bryan/paren ()
  (use-package paren
    :ensure t
    :init
    (progn
      (show-paren-mode 1)
      (setq show-paren-style 'parenthesis)
      ;; (set-face-background 'show-paren-match (face-background 'default))
      ;; (set-face-foreground 'show-paren-match "#def")
      ;; (set-face-attribute 'show-paren-match-face nil :weight 'medium :underline nil)
      (setq show-paren-delay 0))))

(defun bryan/themes ()
  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  ;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux")
  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme")
  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el")
  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/color-theme-ujelly")
  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-darktooth")
  ;;(add-to-list 'load-path "~/.emacs.d/themes/tao-theme-emacs")
  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tao-theme-emacs")

  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe-theme.el/")
  (add-to-list 'load-path "~/.emacs.d/themes/moe-theme.el/")
  ;; so kawaii~  ✿◕ ‿ ◕✿

  ;; USUAL SETUP ;;;;;;;;;;;;
  (require 'moe-theme)
  ;; (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-highlight-buffer-id nil)
  (moe-theme-set-color 'purple)
  ;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
  ;; (moe-light)
  (moe-dark)
  ;; (invert-face 'mode-line)
  ;; (set-face-foreground 'mode-line-buffer-id "#FFFFFF")
  ;; (set-face-bold 'mode-line-buffer-id nil)
  ;; (set-background-color "#ffffff")

  ;; no underlined text! include `:weight 'normal` to get rid of bold
  ;; (but who would wanna do that?)
  ;; another example:
  ;;(load-theme 'tao-yin)
  ;;(mapc
    ;;(lambda (face)
      ;;(set-face-attribute face nil :underline nil :weight 'normal))
    ;;(face-list))

  ;; (set-frame-parameter nil 'background-mode 'dark)
  ;; (load-theme 'cyberpunk t)
  ;; (load-theme 'darktooth t)
  ;; (load-theme 'dichromacy t)
  ;; (load-theme 'tao-yang)
  ;; (load-theme 'leuven)
  ;; (load-theme 'tango)
  ;; (load-theme 'tsdh-light)

  ;; (set-face-foreground 'mode-line "black")
  ;; (set-face-background 'mode-line "white")

  ;; (setq rainbow-delimiters-outermost-only-face-count 1)
  ;; (setq rainbow-delimiters-max-face-count 2)

  ;; Current favorite.
  ;; (load-theme 'tao-yin)
  (rainbow-delimiters-mode)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "PaleTurquoise1")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "ivory1")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "dark sea green")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "plum1")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "wheat2")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "medium purple")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "LightSalmon1")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "yellow green")
  (set-cursor-color "orange")
  (set-face-background 'show-paren-match "white")
  (set-face-foreground 'show-paren-match "black")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  ;;(set-face-background hl-line-face "gray15")

  ;; (load-theme 'tao-yang)
  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face "goldenrod")
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face "dodger blue")
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face "forest green")
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face "orchid")
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face "light slate grey")
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face "dark turquoise")
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face "red4")
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "olive drab")
  )

;; (((((((())))))))

(defun bryan/org ()
  (use-package org
    :ensure org-plus-contrib
    :config
    (progn (require 'ox-bibtex)))

  ;; (require 'org)
  ;;(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp")
  ;;(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/contrib/lisp")

  (setq org-src-fontify-natively t)

  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")))

  (setq org-faces-easy-properties
        '((todo . :background) (tag . :foreground) (priority . :foreground)))

  ;; (setq org-todo-keyword-faces
  ;;       '(("STARTED" . (:foreground "yellow"))))
  (setq org-todo-keyword-faces
        '(("STARTED" . ((,class (:bold t :weight bold :foreground ,yellow :background ,white
                                       :box (:line-width 1 :style none)))))))

  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)

  (require 'bryan-org-bullets)
  (bryan/org-bullets)

  (autoload #'org-present "org-present" nil t)

  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-write)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write)))))

  (use-package toc-org
    :ensure t
    :config
    (progn
      (if (require 'toc-org nil t)
          (add-hook 'org-mode-hook #'toc-org-enable)
        (warn "toc-org not found"))
      (add-to-list 'org-tag-alist '("TOC" . ?T))))

  (require 'ox-latex)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass{article}"
                 ("\\section{%s}" . "\\section*{%s}")))
  ;; (setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
  (setq org-latex-pdf-process (list "latexmk -pdf %f"))

  (defun bryan/journal-entry-with-time-string ()
    (let ((time (format-time-string "*%I:%M:%S %p*")))
      (concat time ": %?")))

  (setq org-capture-templates
        '(
          ("j" "Journal entry" plain
           (file+datetree "~/txt/journal.org")
           (function bryan/journal-entry-with-time-string))

          ("t" "Todo entry" plain
           (file+datetree "~/txt/todo.org")
           "**** TODO %?\n")))
  )

(defun bryan/general ()
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (setq mouse-autoselect-window t)

  (desktop-save-mode t)
  (fset 'yes-or-no-p #'y-or-n-p)

  (electric-indent-mode)
  (setq redisplay-dont-pause                 t
        apropos-do-all                       t
        require-final-newline                t
        save-interprogram-paste-before-kill  t
        mouse-yank-at-point                  t
        system-uses-terminfo                 nil
        ring-bell-function                   'ignore
        backup-directory-alist '(("." . "~/.emacs-backups"))
        scheme-program-name    "/usr/local/bin/mit-scheme"
        custom-file            "~/.emacs.d/custom.el")

  (load custom-file)

  (setq doc-view-continuous t)

  (setq-default fill-column 80)

  ;; Replace default expand command
  (global-set-key (kbd "M-/") #'hippie-expand)

  ;; ლ(ಠ益ಠ)ლ ¡porque!
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; getting errors (when scheme file was opened)
  (semantic-mode 0)

  (bind-key "C-x a r" #'align-regexp)

  (use-package saveplace
    :config
    (progn
      (setq-default save-place t)
      (setq save-place-file "~/.emacs.d/saved-places")))

  (require 'tramp)
  (require 'bind-key))

(defun bryan/interface ()
  (dolist
      ;(mode '(tool-bar-mode scroll-bar-mode))
      ;(mode '(menu-bar-mode tool-bar-mode scroll-bar-mode global-hl-line-mode))
      (mode '(tool-bar-mode scroll-bar-mode global-hl-line-mode))
    (when (fboundp mode) (funcall mode -1)))

  (column-number-mode t)
  ;; (blink-cursor-mode t)
  ;; (display-time)
  ;; (display-battery-mode)

  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)

  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; (set-face-attribute 'default nil
  ;;                     :family "Droid Sans Mono Slashed" :height 140 :weight 'normal)
  ;; (set-face-attribute 'default nil
  ;;                     :family "Monaco" :height 130 :weight 'normal)
  ;; (set-face-attribute 'default nil
  ;;                     :family "Deja Vu Sans Mono" :height 90 :weight 'normal)
  ;; (set-face-attribute 'default nil
  ;;                     :family "Inconsolata" :height 120 :weight 'normal)


  ;; https://github.com/source-foundry/Hack
  (set-face-attribute 'default nil
                      :family "Hack" :height 90 :weight 'normal)
  )

;; `bryan-hydra` will be evaled along with all the other functions
(require 'bryan-hydra)

(defun bryan/paredit ()
  (use-package paredit
    :ensure t
    :config
    (progn

      (add-hook 'paredit-mode-hook #'evil-paredit-mode)

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
        (forward-char -1)))))

(defun bryan/hooks ()
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'prog-mode-hook #'linum-mode)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (add-hook 'text-mode-hook #'turn-on-auto-fill)
  (add-hook 'window-startup-hook #'toggle-frame-maximized))

(defun bryan/evil ()
  (use-package evil-leader
    :config
    (progn
      (global-evil-leader-mode)
      (evil-leader/set-leader ",")))

  (use-package evil
    :ensure t
    :init (evil-mode 1)
    :config
    (progn (setq evil-move-cursor-back nil)
           (define-key evil-normal-state-map (kbd "K") #'evil-previous-line)))

  (use-package evil-surround
    :ensure t
    :init (global-evil-surround-mode 1))

  (use-package evil-anzu
    :ensure t
    :config
    (progn
      (with-eval-after-load 'evil
        (require 'evil-anzu))))

  (add-to-list 'evil-emacs-state-modes 'magit-status-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'debugger-mode))

(defun bryan/avy ()
  (use-package avy
    :ensure t
    :config
    (progn
      (avy-setup-default)

      ;; Input one char, jump to it with a tree.
      (global-set-key (kbd "C-:") #'avy-goto-char)

      ;; Input two consecutive chars, jump to the first one with a tree.
      (global-set-key (kbd "C-'") #'avy-goto-char-2)

      ;; Input zero chars, jump to a line start with a tree.
      (global-set-key (kbd "M-g f") #'avy-goto-line)

      ;; Input one char at word start, jump to a word start with a tree.
      (global-set-key (kbd "M-g w") #'avy-goto-word-1)

      ;; Input zero chars, jump to a word start with a tree.
      (global-set-key (kbd "M-g e") #'avy-goto-word-0)

      (define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
      (define-key evil-motion-state-map (kbd "P") #'avy-goto-line)

      (define-key evil-motion-state-map (kbd "SPC") #'avy-goto-char-2)
      (define-key evil-motion-state-map (kbd "C-SPC") #'avy-goto-word-1)
      (define-key evil-motion-state-map (kbd "M-SPC") #'avy-goto-line))))

(defun bryan/frontend ()
  (use-package web-mode
    :config
    (progn
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-current-element-highlight t)
      ;; (setq web-mode-enable-current-column-highlight t)
      )
    :mode (("\\.html?\\'" . web-mode)
           ("\\.css\\'"   . web-mode)
           ("\\.scss\\'"  . web-mode)))

  (use-package sws-mode
    :mode "\\.styl$")

  (use-package jade-mode
    :mode "\\.jade$")

  (add-hook 'js-mode-hook #'paredit-nonlisp-hook))

(defun bryan/markdown ()
  (use-package markdown-mode
    :mode (("\\.text\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode)))

  (add-hook 'markdown-mode-hook #'pandoc-mode))

(defun bryan/json ()
  (use-package json-mode
    :config
    (progn
      (setq c-basic-offset 2)
      (setq js-indent-level 2)
      (setq json-reformat:indent-width 4))))

(defun bryan/term ()
  (defun zshell ()
    "start a terminal with zshell"
    (interactive)
    (ansi-term "/usr/local/bin/zsh")))

(defun bryan/projectile ()
  (use-package projectile
    :ensure t
    :config (projectile-mode)))

(defun bryan/helm ()
  (use-package helm
    :ensure t
    :config
    (progn
      (setq helm-split-window-in-side-p               t ; open helm buffer inside current window, not occupy whole other window
            helm-move-to-line-cycle-in-source         t ; move to end or beginning of source when reaching top or bottom of source.
            helm-autoresize-max-height                80; it is %.
            helm-autoresize-min-height                20 ; it is %.
            fit-window-to-buffer-horizontally         1
            helm-browse-project-default-find-files-fn #'helm-browse-project-ag-find-files)
      (helm-autoresize-mode 1)
      (helm-projectile-on)
      (helm-mode 1)))
  ;; I don't think these packages actually exist... use-package can't find them
  ;; (use-package helm-adaptive
  ;;   :ensure t
  ;;   :config (helm-adaptive-mode 1))
  ;; (use-package helm-utils
  ;;   :ensure t
  ;;   ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
  ;;   :config (helm-popup-tip-mode 1))
  ;; (use-package helm-sys
  ;;   :ensure t
  ;;   :commands (helm-top)
  ;;   :config (helm-top-poll-mode 1))
  ;; (use-package helm-info
  ;;   :ensure t)
  (use-package helm-ag
    :ensure t)

  (require 'helm-projectile)
  (defun helm-projectile-ag-workspace (&optional options)
    (interactive (if current-prefix-arg (list (helm-read-string "option: " "" 'helm-ag--extra-options-history))))
    (helm-projectile-ag-internal WORKSPACE_ROOT options))

  (require 'helm-swoop))

(defun bryan/ivy ()
  (use-package swiper
    :ensure t
    :config
    (progn
      (ivy-mode 1)
      (setq ivy-use-virtual-buffers t)
      (setq ivy-height 10)
      (setq ivy-count-format "%-4d ")
      (setq ivy-wrap t)
      (setq confirm-nonexistent-file-or-buffer t)))

  (use-package counsel
    :ensure t))

(defun bryan/better-splits ()

  (defun split-vert-and-switch ()
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun split-horiz-and-switch ()
    (interactive)
    (split-window-horizontally)
    (other-window 1)))

(defun bryan/rename-or-delete-buffer-and-file ()

  (defun prelude-rename-buffer-and-file ()
    "Rename current buffer and if the buffer is visiting a file, rename it too."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

  (defun prelude-delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (if (vc-backend filename)
            (vc-delete-file filename)
          (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer)))))))

(defun bryan/minibuffer-keyboard-quit ()

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit))))

(defun bryan/util ()

  (use-package winner
    :ensure t
    :defer t
    :config (winner-mode 1))

  (use-package recentf
    :defer t
    :config
    (progn
      (recentf-mode 1)
      (setq recentf-max-saved-items 200
            recentf-max-menu-items  25)))

  (use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region)))

(defun bryan/flycheck ()
  (use-package flycheck
    :ensure t
    :defer t
    :init (setq-default flycheck-disabled-checkers '(javascript-jshint)))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(defun bryan/c ()
  (defun c-mode-custom-hook ()
    (setq c-default-style "linux")
    (setq c-basic-offset 4)
    (bind-key "C-c C-c" #'recompile c-mode-map))

  ;; (add-hook 'c-mode-common-hook #'paredit-nonlisp-hook)
  (add-hook 'c-mode-common-hook #'c-mode-custom-hook))

(defun bryan/ocaml ()
  (use-package tuareg
    :ensure t)
  (use-package utop
    :ensure t)
  (use-package merlin
    :ensure t)

  ;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                auto-mode-alist))
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))

(defun bryan/haskell ()
  (use-package intero
    :ensure t)
  (add-hook 'haskell-mode-hook #'intero-mode)
  (setq intero-blacklist '("~/.xmonad"))
  (add-hook 'haskell-mode-hook 'intero-mode-blacklist))

(defun bryan/elm ()
  (use-package elm-mode
    :ensure t))

(defun bryan/racket ()
  (setq geiser-active-implementations '(racket))

  (defun racket-mode-custom-hook ()
    (bind-key "s-e" #'geiser-eval-definition scheme-mode-map))

  (add-hook 'geiser-mode-hook #'racket-mode-custom-hook))

(defun bryan/multiple-cursors ()
  ;; https://github.com/magnars/multiple-cursors.el
  (use-package multiple-cursors
    :ensure t
    :defer t
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this))))

(defun bryan/clean-mode-line ()
  (defvar mode-line-cleaner-alist
    `((company-mode           . " Co.")
      (paredit-mode           . " π")
      (inf-haskell-mode       . " λinf")
      (hi2-mode               . " hi2")
      ;; Major modes
      (lisp-interaction-mode  . "λeval")
      (lisp-mode              . "(())")
      (scheme-mode            . "λscm")
      (racket-mode            . "λrkt")
      (clojure-mode           . "λclj")
      (emacs-lisp-mode        . "λel")
      (common-lisp-mode       . "λcl")
      (haskell-mode           . "λ")
      (literate-haskell-mode  . "λlit")
      (inferior-haskell-mode  . "λinf")
      (tuareg-mode            . "λOCaml")
      (python-mode            . "py")
      (doc-view-mode           . "dv")
      ;; hidden
      (helm-mode               . "")
      (undo-tree-mode         . "")
      (magit-auto-revert-mode . "")
      (eldoc-mode             . "")
      (elisp-slime-nav-mode   . "")
      (beacon-mode            . "")
      (which-key-mode         . ""))
    "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

  (defun clean-mode-line ()
    (interactive)
    (loop for cleaner in mode-line-cleaner-alist
          do (let* ((mode (car cleaner))
                    (mode-str (cdr cleaner))
                    (old-mode-str (cdr (assq mode minor-mode-alist))))
               (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
               (when (eq mode major-mode)
                 (setq mode-name mode-str)))))

  (add-hook 'init-hook #'clean-mode-line)
  (add-hook 'after-change-major-mode-hook #'clean-mode-line))

(defun bryan/toggle-split-or-rotate-windows ()

  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    #'split-window-horizontally
                  #'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (defun rotate-windows ()
    "Rotate your windows"
    (interactive)
    (cond ((not (> (count-windows)1))
           (message "You can't rotate a single window!"))
          (t
           (setq i 1)
           (setq numWindows (count-windows))
           (while  (< i numWindows)
             (let* (
                    (w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2))
                    )
               (set-window-buffer w1  b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

(defun bryan/smart-quotes ()

  (defun replace-smart-quotes (beg end)
    "Replace 'smart quotes' in buffer or region with ascii quotes."
    (interactive "r")
    (format-replace-strings '(("\x201C" . "\"")
                              ("\x201D" . "\"")
                              ("\x2018" . "'")
                              ("\x2019" . "'"))
                            nil beg end))

  (defun yank-and-replace-smart-quotes ()
    "Yank (paste) and replace smart quotes from the source with ascii quotes."
    (interactive)
    (yank)
    (replace-smart-quotes (mark) (point)))

  (global-set-key (kbd "C-c y") #'yank-and-replace-smart-quotes))

(defun bryan/scala ()

  (use-package ensime
    :ensure t
    :pin melpa-stable)


  ;; (use-package scala-mode2
  ;;   :ensure t)

  ;; (defun scala-mode-custom-hook ()
  ;;   ;; sbt-find-definitions is a command that tries to find (with grep)
  ;;   ;; the definition of the thing at point.
  ;;   (local-set-key (kbd "M-.") #'sbt-find-definitions)

  ;;   ;; use sbt-run-previous-command to re-compile your code after changes
  ;;   (local-set-key (kbd "C-x '") #'sbt-run-previous-command))

  ;; (add-hook 'scala-mode-hook #'scala-mode-custom-hook)

  ;; (use-package sbt-mode
  ;;   :ensure t)

  ;; (defun sbt-mode-custom-hook ()
  ;;   ;; compilation-skip-threshold tells the compilation minor-mode
  ;;   ;; which type of compiler output can be skipped. 1 = skip info
  ;;   ;; 2 = skip info and warnings.
  ;;   (setq compilation-skip-threshold 1)

  ;;   ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;;   ;; cursor to just after prompt.
  ;;   (local-set-key (kbd "C-a") #'comint-bol)

  ;;   ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;;   ;; more than one line to scala console prompt before sending it
  ;;   ;; for interpretation. It will keep your command history cleaner.
  ;;   (local-set-key (kbd "M-RET") #'comint-accumulate))

  ;; (add-hook 'sbt-mode-hook #'sbt-mode-custom-hook)
  )

(defun bryan/magit ()
  (use-package magit
    :ensure t
    :config
    (progn
      (setq magit-last-seen-setup-instructions "1.4.0")
      (setq magit-status-buffer-switch-function #'switch-to-buffer)
      (setq magit-push-always-verify nil))))

(defun bryan/pandoc ()
  (use-package pandoc-mode
    :ensure t)

  (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings))

(defun bryan/hy ()
  (use-package hy-mode
    :ensure t)

  (add-hook 'hy-mode-hook #'paredit-mode))

(defun bryan/rust ()
  (use-package rust-mode
    :ensure t
    :config
    (progn
      (setq rust-rustfmt-bin "fmt")
      ; This is formatting weird so disable for now...
      ;(rust-enable-format-on-save)
      ))

  (use-package cargo
    :ensure t
    :config
    (progn
      (add-hook 'rust-mode-hook 'cargo-minor-mode)))

  (use-package racer
    :ensure t
    :config
    (progn
      (add-hook 'rust-mode-hook #'racer-mode)
      (add-hook 'racer-mode-hook #'eldoc-mode)

      (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)))
  )

(defun bryan/elisp ()
  ;; Automatically load paredit when editing a lisp file
  ;; More at http://www.emacswiki.org/emacs/ParEdit
  (autoload #'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; eldoc-mode shows documentation in the minibuffer when writing code
  ;; http://www.emacswiki.org/emacs/ElDoc
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

  (require 'elisp-slime-nav))

(defun bryan/clojure ()
  (use-package cider
    :ensure t))

(defun bryan/keybindings ()

  (setq x-meta-keysym 'super)
  (setq x-super-keysym 'meta)

  (global-set-key [(super t)] nil)

  (defun pop-for-avy ()
    (interactive)
    (set-mark-command 4))

  (use-package zygospore
    :ensure t)

  (defun smart-line-beginning ()
    "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
    (interactive)
    (let ((pt (point)))
      (beginning-of-line-text)
      (when (eq pt (point))
        (beginning-of-line))))

  (defun enter-journal-entry ()
    (interactive)
    (org-capture nil "j"))

  (defun enter-todo-entry ()
    (interactive)
    (org-capture nil "t"))

  ;; (evil-leader/set-key
  ;;   ;;"S" #'pop-tag-mark
  ;;   ;;"d" #'elisp-slime-nav-describe-elisp-thing-at-point
  ;;   "r" #'eshell
  ;;   "r"     #'eshell
  ;;   "v"     #'evil-paste-after
  ;;   "c"     #'evil-yank
  ;;   "a"     #'mark-whole-buffer
  ;;   "s"     #'Save-buffer
  ;;   "R"     #'prelude-rename-buffer-and-file
  ;;   "D"     #'prelude-delete-file-and-buffer
  ;;   "i"     #'magit-status
  ;;   "o"     #'other-window
  ;;   "O"     #'other-frame
  ;;   "e"     #'eval-defun
  ;;   "n"     #'next-buffer
  ;;   "p"     #'previous-buffer
  ;;   "P"     #'ns-print-buffer
  ;;   "h"     #'windmove-left
  ;;   "j"     #'windmove-down
  ;;   "k"     #'windmove-up
  ;;   "l"     #'windmove-right
  ;;   "L"     #'goto-line
  ;;   "l"     #'windmove-right
  ;;   "m"     #'org-publish-current-file
  ;;   "q"     #'previous-history-element
  ;;   ;;"d"     #'next-history-element
  ;;   "K"     #'kill-this-buffer
  ;;   "0"     #'delete-window
  ;;   "1"     #'zygospore-toggle-delete-other-windows
  ;;   "2"     #'split-vert-and-switch
  ;;   "3"     #'split-horiz-and-switch
  ;;   "A"     #'evil-copy-from-above
  ;;   "u"     #'revert-buffer
  ;;   "w"     #'counsel-M-x
  ;;   "b"     #'ivy-switch-buffer
  ;;   "f"     #'counsel-find-file
  ;;   ";"     #'erc-track-switch-buffer
  ;;   "SPC"   #'enter-journal-entry
  ;;   ;;"s-SPC" #'enter-todo-entry
  ;;   )

  (bind-keys*
   ("C-s-r"   . zshell)
   ("s-r"     . eshell)
   ("s-v"     . evil-paste-after)
   ("s-c"     . evil-yank)
   ("s-a"     . mark-whole-buffer)
   ("s-s"     . save-buffer)
   ("s-R"     . prelude-rename-buffer-and-file)
   ("s-D"     . prelude-delete-file-and-buffer)
   ("s-i"     . magit-status)
   ("s-o"     . other-window)
   ("s-O"     . other-frame)
   ("s-e"     . eval-defun)
   ("s-n"     . next-buffer)
   ("s-p"     . previous-buffer)
   ("s-P"     . ns-print-buffer)
   ("s-h"     . windmove-left)
   ("s-j"     . windmove-down)
   ("s-k"     . windmove-up)
   ("s-l"     . windmove-right)
   ("s-L"     . goto-line)
   ("s-l"     . windmove-right)
   ("s-m"     . org-publish-current-file)
   ("s-q"     . previous-history-element)
   ("s-d"     . next-history-element)
   ("s-K"     . kill-this-buffer)
   ("C-x 2"   . split-vert-and-switch)
   ("C-x 3"   . split-horiz-and-switch)
   ("s-0"     . delete-window)
   ("s-1"     . zygospore-toggle-delete-other-windows)
   ("s-2"     . split-vert-and-switch)
   ("s-3"     . split-horiz-and-switch)
   ("C-c w"   . bryan/window-stuff-hydra/body)
   ("C-c c"   . bryan/multiple-cursors-hydra/body)
   ("C-c o"   . bryan/org-hydra/body)
   ("C-c a"   . bryan/avy-hydra/body)
   ("C-c m"   . bryan/hydra-hydra/body)
   ("C-c r"   . evil-ranger)
   ("C-;"     . pop-for-avy)
   ("s-A"     . evil-copy-from-above)
   ("s-u"     . revert-buffer)
   ("H-e"     . evil-execute-in-emacs-state)
   ("M-w"     . helm-M-x)
   ("M-x"     . helm-M-x)
   ("s-w"     . helm-M-x)
   ("C-h r"   . helm-info-emacs)
   ("s-b"     . helm-mini)
   ("\C-s"    . helm-swoop)
   ("C-x s"   . helm-projectile-ag)
   ("C-c s"   . helm-projectile-ag-workspace)
   ;("C-c C-r" . ivy-resume)
   ("C-x C-f" . helm-find-files)
   ("s-f"     . helm-find-files)
   ;("C-h f"   . counsel-describe-function)
   ;("C-h v"   . counsel-describe-variable)
   ;("<f2> u"  . counsel-unicode-char)
   ;("C-c g"   . counsel-git)
   ;("C-c j"   . counsel-git-grep)
   ;("C-x l"   . counsel-locate)
   ("s-;"     . erc-track-switch-buffer)
   ("C-s-n"   . eyebrowse-next-window-config)
   ("C-s-p"   . eyebrowse-prev-window-config)
   ("M-0"     . eyebrowse-switch-to-window-config-0)
   ("M-1"     . eyebrowse-switch-to-window-config-1)
   ("M-2"     . eyebrowse-switch-to-window-config-2)
   ("M-3"     . eyebrowse-switch-to-window-config-3)
   ("M-4"     . eyebrowse-switch-to-window-config-4)
   ("M-5"     . eyebrowse-switch-to-window-config-5)
   ("M-6"     . eyebrowse-switch-to-window-config-6)
   ("M-7"     . eyebrowse-switch-to-window-config-7)
   ("M-8"     . eyebrowse-switch-to-window-config-8)
   ("M-9"     . eyebrowse-switch-to-window-config-9)
   ("C-x C-m" . compile)
   ("s-SPC"   . enter-journal-entry)
   ("S-s-SPC" . enter-todo-entry)
   ("M-s-;"   . comment-or-uncomment-region)
   ;("<f6>"   . ivy-resume)
   ;("<f1> l" . counsel-load-library)
   ;("<f2> i" . counsel-info-lookup-symbol)
   ;("C-c k"  . counsel-ag)
   ;("C-S-o"  . counsel-rhythmbox)
   )

  ;; maybe
  ;; (setcdr evil-insert-state-map nil)
  ;; (define-key evil-insert-state-map [escape] #'evil-normal-state)

  (bind-keys :map evil-motion-state-map ("C-a" . smart-line-beginning))
  (bind-keys :map evil-insert-state-map ("C-a" . smart-line-beginning))
  (bind-keys :map evil-visual-state-map ("C-a" . smart-line-beginning))
  (bind-keys :map evil-normal-state-map ("C-a" . smart-line-beginning))

  (bind-keys :map evil-motion-state-map ("C-e" . move-end-of-line))
  (bind-keys :map evil-insert-state-map ("C-e" . move-end-of-line))
  (bind-keys :map evil-visual-state-map ("C-e" . move-end-of-line))
  (bind-keys :map evil-normal-state-map ("C-e" . move-end-of-line))
  ;; ("C-e" . evil-copy-from-below)

  (define-key evil-normal-state-map [escape] #'keyboard-quit)
  (define-key evil-visual-state-map [escape] #'keyboard-quit)
  (define-key minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)
  (global-set-key [escape] #'evil-exit-emacs-state)

  (bind-key "M-)" #'paredit-wrap-round-from-behind evil-motion-state-map)
  (bind-key "M-)" #'paredit-wrap-round-from-behind evil-insert-state-map)

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)
    (evil-leader/set-key
      "s" #'elisp-slime-nav-find-elisp-thing-at-point
      "S" #'pop-tag-mark
      "d" #'elisp-slime-nav-describe-elisp-thing-at-point))

  (add-hook 'term-mode-hook #'(lambda ()
                                (define-key term-raw-map (kbd "s-v") #'term-paste)))

  (define-key org-mode-map "\C-ck" #'endless/insert-key)
  (defun endless/insert-key (key)
    "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((is-org-mode (derived-mode-p 'org-mode))
           (tag (if is-org-mode
                    "@@html:<kbd>%s</kbd>@@"
                  "<kbd>%s</kbd>")))
      (if (null (equal key "\r"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char (if is-org-mode -8 -6))))))

(defun bryan/eval-fns ()
  (bryan/company)
  (bryan/paren)
  (bryan/themes)
  (bryan/org)
  (bryan/general)
  (bryan/interface)
  (bryan/hydra)
  (bryan/paredit)
  (bryan/hooks)
  (bryan/evil)
  (bryan/avy)
  (bryan/frontend)
  (bryan/markdown)
  (bryan/json)
  (bryan/term)
  (bryan/projectile)
  (bryan/helm)
  (bryan/better-splits)
  (bryan/rename-or-delete-buffer-and-file)
  (bryan/minibuffer-keyboard-quit)
  (bryan/util)
  (bryan/flycheck)
  (bryan/c)
  (bryan/ocaml)
  (bryan/haskell)
  (bryan/elm)
  (bryan/racket)
  (bryan/multiple-cursors)
  (bryan/clean-mode-line)
  (bryan/toggle-split-or-rotate-windows)
  (bryan/smart-quotes)
  (bryan/scala)
  (bryan/magit)
  (bryan/pandoc)
  (bryan/hy)
  (bryan/rust)
  (bryan/elisp)
  (bryan/clojure)
)

(bryan/eval-fns)

(global-set-key [s-mouse-1] #'browse-url-at-mouse)

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
    (add-hook 'sql-mode-hook #'sqlup-mode)
    ;; Capitalize keywords in an interactive session (e.g. psql)
    (add-hook 'sql-interactive-mode-hook #'sqlup-mode)
    ;; Set a global keyword to use sqlup on a region
    ;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
    ))

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))
(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          #'(lambda () (local-set-key "o" #'my-gnus-group-list-subscribed-groups)))

(use-package reveal-in-osx-finder
  ;; To load at the start up
  :ensure t
  ;; If you want to configure a keybinding (e.g., C-c z), add the following
  ;; :config
  ;; (progn
  ;;   (global-set-key (kbd "C-c z") #'reveal-in-osx-finder))
  )

(use-package erlang
  :ensure t)
;; is this doing anything? I don't know if I have this on somewhere already...
(setq doc-view-continuous t)

;;;;;; in progress

(defun duplicate-and-comment-line-or-region (&optional beg end)
  (interactive "r")
  (setq temp-point (point))
  (if mark-active
      (copy-and-comment-region)
    (duplicate-and-comment-line))
  (goto-char temp-point))

(defun duplicate-and-comment-line ()
  (kill-whole-line)
  (yank)
  (yank)
  (previous-line)
  (comment-region (line-beginning-position) (line-end-position))
  (previous-line))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun copy-and-comment-region (beg end)
  "Duplicate the region and comment-out the copied text.
See `comment-region' for behavior of a prefix arg."
  (interactive "r")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region (+ (- end beg) beg) (+ (- end beg) end)))

(use-package bug-hunter :ensure t)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
;; maybe...
;; (add-hook 'text-mode-hook #'remove-dos-eol)

(add-to-list 'evil-insert-state-modes #'haskell-interactive-mode)

;; http://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :config
  (progn

    (defun neotree-project-dir ()
      "Open NeoTree using the git root."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (neotree-toggle)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)))
          (message "Could not find git project root."))))

    (global-set-key [f8] #'neotree-project-dir)
    (setq neo-smart-open t)
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (define-key evil-normal-state-local-map (kbd "TAB") #'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") #'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "q") #'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") #'neotree-enter)))

(setq TeX-save-query nil)

(use-package beacon
  :ensure t
  :config
  (progn
    (beacon-mode 1)
    (setq beacon-blink-duration 0.5)))

(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode 1)
    (setq popwin:popup-window-position 'right)
    ;; (setq popwin:popup-window-height 15) ; 15 is the default value
    (setq popwin:popup-window-width 80)))

;; Plan 9 Smart Shell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
;; for some reason it doesn't work w/o vvvvvvv
(eshell-smart-initialize)

(require 'erc)
(setq erc-server-coding-system '(utf-8 . utf-8))
(setq erc-timestamp-format "[%I:%M %p]")
(setq erc-hide-timestamps t)
(setq erc-echo-timestamps nil)
(setq erc-echo-timestamp-format "TS'd %A, %I:%M:%S %p")
(setq erc-track-showcount t)
(setq erc-track-enable-keybindings t)
(add-to-list 'erc-modules 'scrolltobottom)

;; Don't recenter
(defun bryan/erc-scroll-conservatively ()
  (set (make-local-variable 'scroll-conservatively) 100))

(defun bryan/erc-echo-timestamps-only-in-erc-buffers ()
  (make-local-variable 'erc-echo-timestamps)
  (setq erc-echo-timestamps t))

(add-hook 'erc-mode-hook #'bryan/erc-scroll-conservatively)
(add-hook 'erc-mode-hook #'bryan/erc-echo-timestamps-only-in-erc-buffers)

(add-to-list 'erc-modules 'move-to-prompt)
(add-to-list 'erc-modules 'ring)

;; (erc-update-modules)
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
;; To also exclude messages sent by the server when you join a channel, such as
;; the nicklist and topic:
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; From erc.el:
;; (MODE   . "%n (%u@%h) has changed mode for %t to %m")
;; (s332   . "Topic for %c: %T")
;; (s333   . "%c: topic set by %n, %t")
;; (s353   . "Users on %c: %u")
;; (s328   . "%c URL: %u")
;; (s324   . "%c modes: %m")
;; (s329   . "%c was created on %t")
(setq erc-hide-list '("JOIN" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353"))

(defun bryan/erc-channel-info (title msg &optional extra)
  (let ((channel (buffer-name)))
    (with-output-to-temp-buffer (format "*%s Info: %s*" channel title)
      (princ msg)
      (if extra
          (princ (format "\n\n%s" extra))))))

(defun bryan/erc-channel-users ()
  (interactive)
  (let* ((xs    (hash-table-keys erc-channel-users))
         (users (format "%s" xs))
         (count (length xs))
         (extra (format "Total users: %d" count)))
    (bryan/erc-channel-info "Users" users extra)))

(defun bryan/erc-channel-topic ()
  (interactive)
  (bryan/erc-channel-info "Topic" erc-channel-topic))

;; (setq erc-log-p nil)

;; (setq erc-track-exclude '("*status"
;;                           "#haskell"
;;                           "#emacs"
;;                           "##prolog"
;;                           "#emacs-beginners"
;;                           "#haskell-beginners"
;;                           "#ocaml"))
;; (setq erc-track-exclude nil)

(setq erc-current-nick-highlight-type 'nick)
(setq erc-track-faces-priority-list '(;;erc-error-face
                                      erc-current-nick-face
                                      ;;erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      ;; erc-dangerous-host-face
                                      ;;erc-notice-face
                                      ;;erc-prompt-face
                                      ))

;; (-difference '(a b c d e f) '(a c)) ; (b d e f)
(defun quieter-erc ()
  "Minimal distraction for all channels except important ones
which are defined in ~/.private.el"
  (interactive)
  (setq erc-track-priority-faces-only
        (-difference (bryan/erc-joined-channels) erc-important-chans)))

;; (setq erc-track-priority-faces-only nil)
(setq erc-track-exclude-server-buffer t)

(defun bryan/erc-list-channels-within-buffer (chanbuf)
  (with-current-buffer chanbuf (erc-default-target)))

(defun bryan/erc-joined-channels ()
  "Return all the channels you're in as a list.  This does not include queries."
  (save-excursion
    ;; need to get out of ERC mode so we can have *all* channels returned
    (set-buffer "*scratch*")
    (mapcar #'bryan/erc-list-channels-within-buffer
            (erc-channel-list erc-process))))

(setq erc-join-buffer 'bury)
;; (use-package erc-hl-nicks
;;   :ensure t)

(setq erc-fill-function #'erc-fill-static)
(setq erc-fill-static-center 15)

(defun my-erc-terminal-notifier ()
  (defvar erc-terminal-notifier-command nil "The path to terminal-notifier.")
  (setq erc-terminal-notifier-command (executable-find "terminal-notifier"))

  (defun erc-terminal-notifier-notify (title message)
    "Show a message with `terminal-notifier-command`."
    (start-process "terminal-notifier"
                   "*terminal-notifier*"
                   erc-terminal-notifier-command
                   "-title" title
                   "-message" message
                   "-activate" "org.gnu.Emacs"
                   "-sender" "org.gnu.Emacs"))

  (defun erc-terminal-notifier-text-matched (match-type nick message)
    "Show a notification, when user's nick is mentioned."
    (let ((cur-buf (buffer-name (current-buffer))))
      (when (eq match-type 'current-nick)
        (unless (or (posix-string-match "^\\** *Users on #" message)
                    (posix-string-match "^\\*irc*" cur-buf))
          (erc-terminal-notifier-notify
           (concat "ERC " cur-buf)
           (concat "\\<" (nth 0 (erc-parse-user nick)) "> " message))))))

  (if (eq system-type 'darwin)
      (add-hook 'erc-text-matched-hook #'erc-terminal-notifier-text-matched)))

(my-erc-terminal-notifier)

(define-key key-translation-map (kbd "<M-down-mouse-1>") (kbd "<down-mouse-2>"))

(use-package znc
  :ensure t)

(use-package slime
  :ensure t
  :config
  (progn
    ;; Set your lisp system and, optionally, some contribs
    (setq inferior-lisp-program "/usr//bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

(require 'midnight)

(use-package golden-ratio
  :ensure t
  ;; :config
  ;; (progn
  ;;   (golden-ratio-mode 1))
  )

(load "~/.emacs.d/lisp/html-lite.el/html-lite")
(load "~/txt/bryangarza.github.io/lisp/blog.el")
(bryan/blog)

(use-package fill-column-indicator
  :ensure t
  ;; :config
  ;; (progn
  ;;   (add-hook 'prog-mode-hook #'fci-mode)
  ;;   (add-hook 'text-mode-hook #'fci-mode)
  ;;   (add-hook 'org-mode-hook  #'fci-mode)

  ;;   ;; advising org-html-fontify-code because otherwise fci-mode gets in the way
  ;;   ;; of the exports and insert garbage characters
  ;;   ;; https://github.com/alpaker/Fill-Column-Indicator/issues/45#issuecomment-108911964
  ;;   (defun fci-mode-override-advice (&rest args))
  ;;   (advice-add 'org-html-fontify-code :around
  ;;               (lambda (fun &rest args)
  ;;                 (advice-add 'fci-mode :override #'fci-mode-override-advice)
  ;;                 (let ((result  (apply fun args)))
  ;;                   (advice-remove 'fci-mode #'fci-mode-override-advice)
  ;;                   result))))
  )

;; (use-package diff-hl
;;   :ensure t
;;   :config
;;   (progn
;;     (remove-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
;;     (global-diff-hl-mode)
;;     (diff-hl-flydiff-mode)))

(defun endless/sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'endless/sharp)

(use-package eyebrowse
  :ensure t
  :config
  (progn
    ;; (eyebrowse-mode t)
    (setq eyebrowse-wrap-around t)
    (eyebrowse-setup-evil-keys)
    (setq eyebrowse-switch-back-and-forth t)))

(use-package stripe-buffer
  :ensure t
  :config
  (progn
    ;; (set-face-background 'stripe-highlight "white smoke") ; light
    (set-face-background 'stripe-highlight "#222222") ; dark
    (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
    ))

;(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;(load-file "~/.emacs.d/lisp/ProofGeneral-4.3pre150930/ProofGeneral-4.3pre150930/generic/proof-site.el")

;; (use-package company-coq
;;  :ensure t
;;  :config (add-hook 'coq-mode-hook #'company-coq-mode))

(bryan/keybindings)

(setq compilation-read-command nil)
;;(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq diary-file "~/txt/diary")

(use-package lua-mode
  :ensure t
  :config
  (progn
    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

(use-package htmlize
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package go-mode
  :ensure t)

(use-package company-go
  :ensure t)

(use-package protobuf-mode
  :ensure t)
