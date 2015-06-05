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

;; (add-to-list 'load-path "~/.emacs.d/lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/lisp/haskell-mode/")

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

;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (setq js2-strict-missing-semi-warning nil)

(use-package json-mode
  :config
  (progn
    (setq c-basic-offset 2)
    (setq js-indent-level 2)
    (setq json-reformat:indent-width 4)))

(require 'bryan-helm)

(defun split-vert-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-horiz-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

;; getting errors (when scheme file was opened)
(semantic-mode 0)

(require 'bryan-keybindings)

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

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

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

;;; OCaml setup starts here
(load "/Users/bryangarza/.emacs.d/lisp/tuareg/tuareg-site-file.el")

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(use-package merlin
  :init
  (progn
    ;; Enable auto-complete
    (setq merlin-use-auto-complete-mode 'easy)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;; Start merlin on OCaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

(add-to-list 'load-path "/Users/bryangarza/.opam/system/share/emacs/site-lisp")

(require 'ocp-indent)

;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;; Update the emacs load path
(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)

(defun ocaml-custom-hook ()
  (paredit-mode 1))

;; (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;; (add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'ocaml-custom-hook)

;;; OCaml setup ends here

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

;; For later: https://wiki.haskell.org/Emacs/Indentation#Aligning_code
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

(global-set-key (kbd "C-c y") 'yank-and-replace-smart-quotes)

(use-package scala-mode2
  :ensure t)

(defun scala-mode-custom-hook ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command))

(add-hook 'scala-mode-hook 'scala-mode-custom-hook)

(use-package sbt-mode
  :ensure t)

(defun sbt-mode-custom-hook ()
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "M-RET") 'comint-accumulate))

(add-hook 'sbt-mode-hook 'sbt-mode-custom-hook)

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

(require 'bryan-rename-or-delete-buffer-and-file)
