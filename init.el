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

(defvar bryan/packages
  '(evil-paredit
    smartparens
    rainbow-delimiters
    rainbow-mode
    clojure-mode-extra-font-locking
    cider
    exec-path-from-shell
    debbugs
    auto-complete
    geiser
    ac-geiser
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

;; (load-file "~/.private.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(eval-when-compile
  (require 'use-package))

(defun bryan-company ()
  (use-package company
    :ensure t))

(defun bryan-paren ()
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

(defun bryan-themes ()
  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))

  ;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/color-theme-ujelly")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-darktooth")
  ;; (use-package tao-theme
  ;;   :ensure t)

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe-theme.el/")
  (add-to-list 'load-path "~/.emacs.d/themes/moe-theme.el/")
  ;; so kawaii~  ✿◕ ‿ ◕✿

  ;; USUAL SETUP ;;;;;;;;;;;;
  (require 'moe-theme)
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-highlight-buffer-id nil)
  (moe-theme-set-color 'purple)
  ;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
  ;; (moe-light)
  (moe-dark)
  ;; (set-background-color "#ffffff")

  ;; no underlined text! include `:weight 'normal` to get rid of bold
  ;; (but who would wanna do that?)
  ;; another example: (set-face-bold-p 'bold nil)

  ;; (mapc
  ;;   (lambda (face)
  ;;     (set-face-attribute face nil :underline nil))
  ;;   (face-list))

  ;; (set-frame-parameter nil 'background-mode 'dark)
  ;; (load-theme 'cyberpunk t)
  ;; (load-theme 'darktooth t)
  ;; (load-theme 'dichromacy t)
  ;; (load-theme 'tao-yang)
  ;; (load-theme 'leuven)

  ;; (setq rainbow-delimiters-outermost-only-face-count 1)
  ;; (setq rainbow-delimiters-max-face-count 2)
  )

(defun bryan-org ()
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

;;; org-bullets.el --- Show bullets in org-mode as UTF-8 characters
;;; Version: 0.2.4
;;; Author: sabof
;;; URL: https://github.com/sabof/org-bullets
  (defgroup org-bullets nil
    "Display bullets as UTF-8 characters"
    :group 'org-appearance)

  ;; A nice collection of unicode bullets:
  ;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
  (defcustom org-bullets-bullet-list
    '(;;; Large
      "◉"
      "○"
      "✸"
      "✿"
      ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
      ;; ► • ★ ▸
      )
    "This variable contains the list of bullets.
It can contain any number of symbols, which will be repeated."
    :group 'org-bullets
    :type '(repeat (string :tag "Bullet character")))

  (defcustom org-bullets-face-name nil
    "This variable allows the org-mode bullets face to be
 overridden. If set to a name of a face, that face will be
 used. Otherwise the face of the heading level will be used."
    :group 'org-bullets
    :type 'symbol)

  (defvar org-bullets-bullet-map
    '(keymap
      (mouse-1 . org-cycle)
      (mouse-2
       . (lambda (e)
           (interactive "e")
           (mouse-set-point e)
           (org-cycle))))
    "Mouse events for bullets.
Should this be undesirable, one can remove them with

\(setcdr org-bullets-bullet-map nil\)")

  (defun org-bullets-level-char (level)
    (string-to-char
     (nth (mod (1- level)
               (length org-bullets-bullet-list))
          org-bullets-bullet-list)))

;;;###autoload
  (define-minor-mode org-bullets-mode
    "UTF8 Bullets for org-mode"
    nil nil nil
    (let* (( keyword
             `(("^\\*+ "
                (0 (let* (( level (- (match-end 0) (match-beginning 0) 1))
                          ( is-inline-task
                            (and (boundp 'org-inlinetask-min-level)
                                 (>= level org-inlinetask-min-level))))
                     (compose-region (- (match-end 0) 2)
                                     (- (match-end 0) 1)
                                     (org-bullets-level-char level))
                     (when is-inline-task
                       (compose-region (- (match-end 0) 3)
                                       (- (match-end 0) 2)
                                       (org-bullets-level-char level)))
                     (when (facep org-bullets-face-name)
                       (put-text-property (- (match-end 0)
                                             (if is-inline-task 3 2))
                                          (- (match-end 0) 1)
                                          'face
                                          org-bullets-face-name))
                     (put-text-property (match-beginning 0)
                                        (- (match-end 0) 2)
                                        'face (list :foreground
                                                    (face-attribute
                                                     'default :background)))
                     (put-text-property (match-beginning 0)
                                        (match-end 0)
                                        'keymap
                                        org-bullets-bullet-map)
                     nil))))))
      (if org-bullets-mode
          (progn
            (font-lock-add-keywords nil keyword)
            (font-lock-fontify-buffer))
        (save-excursion
          (goto-char (point-min))
          (font-lock-remove-keywords nil keyword)
          (while (re-search-forward "^\\*+ " nil t)
            (decompose-region (match-beginning 0) (match-end 0)))
          (font-lock-fontify-buffer))
        )))

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (autoload 'org-present "org-present" nil t)

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
          (add-hook 'org-mode-hook 'toc-org-enable)
        (warn "toc-org not found"))
      (add-to-list 'org-tag-alist '("TOC" . ?T))))

  (require 'ox-latex)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass{article}"
                 ("\\section{%s}" . "\\section*{%s}")))
  (require 'ox-bibtex)
  ;; (setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
  (setq org-latex-pdf-process (list "latexmk -pdf %f")))

(defun bryan-general ()
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

  (load custom-file)

  (setq doc-view-continuous t)

  (setq-default fill-column 80)

  ;; Replace default expand command
  (global-set-key (kbd "M-/") 'hippie-expand)

  ;; ლ(ಠ益ಠ)ლ ¡porque!
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;; getting errors (when scheme file was opened)
  (semantic-mode 0)

  (bind-key "C-x a r" 'align-regexp)

  (use-package saveplace
    :config
    (progn
      (setq-default save-place t)
      (setq save-place-file "~/.emacs.d/saved-places")))

  (require 'auto-complete)
  (require 'tramp)
  (require 'bind-key))

(defun bryan-interface ()
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode global-hl-line-mode))
    (when (fboundp mode) (funcall mode -1)))

  (column-number-mode t)
  ;; (blink-cursor-mode t)
  ;; (display-time)
  ;; (display-battery-mode)

  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace nil)

  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; (set-face-attribute 'default nil
  ;;                     :family "Droid Sans Mono Slashed" :height 140 :weight 'normal)
  (set-face-attribute 'default nil
                      :family "Monaco" :height 130 :weight 'normal))

(defun bryan-hydra ()
  (defun bryan-define-hydras ()

    (defun hydra-move-splitter-left (arg)
      "Move window splitter left."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (shrink-window-horizontally arg)
        (enlarge-window-horizontally arg)))

    (defun hydra-move-splitter-right (arg)
      "Move window splitter right."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (enlarge-window-horizontally arg)
        (shrink-window-horizontally arg)))

    (defun hydra-move-splitter-up (arg)
      "Move window splitter up."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (enlarge-window arg)
        (shrink-window arg)))

    (defun hydra-move-splitter-down (arg)
      "Move window splitter down."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (shrink-window arg)
        (enlarge-window arg)))

    (defhydra bryan-window-stuff-hydra (:hint nil)
      "
          Split: _v_ert  _s_:horz
     Reorganize: _t_oggle split  _w_indow rotate
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right"

      ;; ("v" split-window-right)
      ;; ("s" split-window-below)
      ("v" split-vert-and-switch)
      ("s" split-horiz-and-switch)

      ("t" toggle-window-split)
      ("w" rotate-windows)

      ("c" delete-window)
      ("o" delete-other-windows :exit t)

      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)

      ("p" previous-buffer)
      ("n" next-buffer)
      ("b" ido-switch-buffer)
      ("f" ido-find-file)
      ;; ("F" projectile-find-file)

      ("u" winner-undo)
      ("r" winner-redo)

      ("H" hydra-move-splitter-left)
      ("J" hydra-move-splitter-down)
      ("K" hydra-move-splitter-up)
      ("L" hydra-move-splitter-right)

      ("q" nil))

    (defhydra bryan-multiple-cursors-hydra (:hint nil)
      "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))

(defhydra bryan-org-hydra (:color red :hint nil)
  "
Navigation^
---------------------------------------------------------
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t)
  ("q" nil))

(defhydra hydra-org (:color red :hint nil)
  "
Navigation^
---------------------------------------------------------
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t)
  ("q" nil))

(defhydra bryan-avy-hydra (:color blue)
  "avy-goto"
  ("c" avy-goto-char "char")
  ("C" avy-goto-char-2 "char-2")
  ("l" avy-goto-line "line")
  ("w" avy-goto-word-1 "word")
  ("W" avy-goto-word-0 "word-0")
  ("q" nil))

(defhydra bryan-hydra-hydra (:color blue)
  "
Hydra for hydras
----------------
[_w_] Window stuff
[_c_] Multiple cursors
[_o_] Org mode
[_a_] Avy
"
("w" bryan-window-stuff-hydra/body)
("c" bryan-multiple-cursors-hydra/body)
("o" bryan-org-hydra/body)
("a" bryan-avy-hydra/body)
("q" nil)))

(use-package hydra
  :ensure t
  :config
  (progn
    (bryan-define-hydras))))

(defun bryan-paredit ()
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
        (forward-char -1)))))

(defun bryan-hooks ()
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'window-startup-hook 'toggle-frame-maximized))

(defun bryan-evil ()
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
           (define-key evil-normal-state-map (kbd "K") 'evil-previous-line)))

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
  (add-to-list 'evil-emacs-state-modes 'debugger-mode))

(defun bryan-avy ()
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
      (define-key evil-motion-state-map (kbd "P") #'avy-goto-line)

      (define-key evil-motion-state-map (kbd "SPC") #'avy-goto-char-2)
      (define-key evil-motion-state-map (kbd "C-SPC") #'avy-goto-word-1)
      (define-key evil-motion-state-map (kbd "M-SPC") #'avy-goto-line))))

(defun bryan-cider ()
  ;; Cider settings
  ;; Enable `eldoc` in Clojure buffers
  (add-hook 'cider-mode-hool #'eldoc-mode)
  ;; Log communication with the nREPL server
  (setq nrepl-log-messages t)
  ;; Hide *nrepl-connection* and *nrepl-server* from some buffer switching
  ;; comands like `switch-to-buffer` (C-x b)
  (setq nrepl-hide-special-buffers t)
  ;; Enable paredit in the REPL buffer
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(defun bryan-auto-complete ()
  (use-package auto-complete
    :ensure t
    :defer t
    :config
    (progn
      (require 'auto-complete-config)
      (auto-complete-mode t))))

(defun bryan-frontend ()
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

  (add-hook 'js-mode-hook 'paredit-nonlisp-hook))

(defun bryan-markdown ()
  (use-package markdown-mode
    :mode (("\\.text\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)
           ("\\.md\\'" . markdown-mode)))

  (add-hook 'markdown-mode-hook 'pandoc-mode))

(defun bryan-json ()
  (use-package json-mode
    :config
    (progn
      (setq c-basic-offset 2)
      (setq js-indent-level 2)
      (setq json-reformat:indent-width 4))))

(defun bryan-term ()
  (defun zshell ()
    "start a terminal with zshell"
    (interactive)
    (ansi-term "/usr/local/bin/zsh")))

(defun bryan-helm ()
  (use-package helm
    :init
    (progn
      (require 'helm-config)
      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))
      (setq
       helm-candidate-number-limit           100
       helm-autoresize-max-height            80 ; it is %.
       helm-scroll-amount                    8
       helm-split-window-in-side-p           t
       helm-move-to-line-cycle-in-source     t
       helm-ff-search-library-in-sexp        t
       helm-ff-file-name-history-use-recentf t
       helm-M-x-fuzzy-match                  t
       helm-quick-update                     t
       helm-bookmark-show-location           t
       helm-buffers-fuzzy-matching           t
       helm-apropos-fuzzy-match              t
       helm-recentf-fuzzy-match              t
       helm-locate-fuzzy-match               t
       helm-file-cache-fuzzy-match           t
       helm-semantic-fuzzy-match             t
       helm-imenu-fuzzy-match                t
       helm-lisp-fuzzy-completion            t)
      (helm-mode)
      (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
      (ido-mode -1) ; just in case
      (helm-autoresize-mode t)
      (global-unset-key (kbd "C-x c"))
      (bind-keys :map helm-map
                 ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
                 ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
                 ("C-z"   . helm-select-action))            ; list actions using C-z
      (bind-key "C-c C-l" 'helm-comint-input-ring shell-mode-map)
      (bind-key "C-c C-l" 'helm-minibuffer-history minibuffer-local-map))
    :bind (("C-c h"     . helm-command-prefix)
           ("M-x"       . helm-M-x)
           ("s-m"       . helm-man-woman)
           ("M-y"       . helm-show-kill-ring)
           ("C-x b"     . helm-mini)
           ("C-x C-f"   . helm-find-files)
           ("s-f"       . helm-find-files)
           ("s-b"       . helm-mini)
           ("C-h SPC"   . helm-all-mark-rings)
           ("C-c h M-:" . helm-eval-expression-with-eldoc)
           ("C-c h o"   . helm-occur)
           ("s-F"       . helm-occur)))

  ;; (require 'helm-eshell)
  ;; (add-hook 'eshell-mode-hook
  ;;               #'(lambda ()
  ;;                   (bind-key "C-c C-l" 'helm-eshell-history eshell-mode-map)))

  ;; (use-package ac-helm
  ;;   :defer t
  ;;   :config (bind-key "C-:" 'ac-complete-with-helm ac-complete-mode-map)
  ;;   :bind ("C-:" . ac-complete-with-helm))

  ;; (use-package helm-descbinds
  ;;   :defer t
  ;;   :bind (("C-h b" . helm-descbinds)
  ;;          ("C-h w" . helm-descbinds)))
  )

(defun bryan-better-splits ()

  (defun split-vert-and-switch ()
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun split-horiz-and-switch ()
    (interactive)
    (split-window-horizontally)
    (other-window 1)))

(defun bryan-rename-or-delete-buffer-and-file ()

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

(defun bryan-minibuffer-keyboard-quit ()

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit))))

(defun bryan-util ()

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

(defun bryan-flycheck ()
  (use-package flycheck
    :ensure t
    :defer t
    :init (setq-default flycheck-disabled-checkers '(javascript-jshint)))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(defun bryan-c ()
  (defun c-mode-custom-hook ()
    (setq c-default-style "linux")
    (setq c-basic-offset 4)
    (bind-key "C-c C-c" 'recompile c-mode-map))

  (add-hook 'c-mode-common-hook 'paredit-nonlisp-hook)
  (add-hook 'c-mode-common-hook 'c-mode-custom-hook))

(defun bryan-ocaml ()

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
  (add-hook 'tuareg-mode-hook 'ocaml-custom-hook))

(defun bryan-haskell ()
  ;; (add-to-list 'load-path "~/.emacs.d/lisp/haskell-mode/")
  ;; (require 'haskell-mode-autoloads)
  ;; (add-to-list 'Info-default-directory-list "~/.emacs.d/lisp/haskell-mode/")

  ;; (use-package hi2
  ;;   :ensure t)

  (defun haskell-custom-hook ()
    (require 'haskell-interactive-mode)
    (require 'haskell-process)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    ;; Getting tired of these 2 sometimes
    ;; (flycheck-mode)
    ;; (paredit-mode 1)
    ;; (turn-on-haskell-simple-indent)
    ;; (turn-on-haskell-indent)
    (haskell-indentation-mode)
    ;; (turn-on-hi2)
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
      ;; "s" 'haskell-mode-tag-find
      )
    ;; Jump to the imports. Keep tapping to jump between import
    ;; groups. C-u f8 to jump back again.
    (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

    (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    ;; ;; Interactively choose the Cabal command to run.
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

    (custom-set-variables
     '(haskell-process-type 'stack-ghci)
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))

    (setq haskell-interactive-popup-errors nil))

  ;; (use-package ghc
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (autoload 'ghc-init "ghc" nil t)
  ;;     (autoload 'ghc-debug "ghc" nil t)))

  ;; (use-package company-ghc
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (add-to-list 'company-backends 'company-ghc)))

  (use-package haskell-mode
    :ensure t
    :config
    (progn
      (bind-key "C-c C-c" 'haskell-compile haskell-mode-map)
      ;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
      ))

  (add-hook 'haskell-mode-hook 'haskell-custom-hook))

(defun bryan-elm ()
  (use-package elm-mode
    :ensure t))

(defun bryan-circe ()
  (use-package circe
    :ensure t
    :config
    (progn
      (setq circe-network-options
            `(("Freenode"
               :tls t
               :service 6697
               :nick ,freenode-username
               :nickserv-password ,freenode-password
               :channels ("#haskell" "#emacs"))))
      (setq circe-reduce-lurker-spam t)
      (setq circe-server-killed-confirmation 'ask-and-kill-all)
      (add-hook 'circe-server-mode-hook
                '(lambda ()
                   (setq-default show-trailing-whitespace nil)))
      (setq circe-default-part-message "...")
      (setq circe-default-quit-message "...")
      (use-package lui-autopaste
        :init
        (progn
          (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
          (defun my-circe-set-margin ()
            (setq right-margin-width 5))
          (add-hook 'lui-mode-hook 'my-circe-set-margin)
          (setq
           lui-time-stamp-position 'right-margin
           lui-time-stamp-format "%H:%M"))))))

(defun bryan-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :bind (("M-i" . helm-swoop)
           ("M-I" . helm-swoop-back-to-last-point)
           ("C-c M-i" . helm-multi-swoop)
           ("C-x M-i" . helm-multi-swoop-all))
    :config
    (progn
      (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
      (bind-key "M-i" 'helm-swoop-from-evil-search evil-motion-state-map)
      (bind-keys :map helm-swoop-map
                 ("M-i" . helm-multi-swoop-all-from-helm-swoop)
                 ("C-r" . helm-previous-line)
                 ("C-s" . helm-next-line))
      (bind-keys :map helm-multi-swoop-map
                 ("C-r" . helm-previous-line)
                 ("C-s" . helm-next-line))
      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t
            ;; If this value is t, split window inside the current window
            helm-swoop-split-with-multiple-windows nil
            ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
            helm-swoop-split-direction 'split-window-horizontally
            ;; If nil, you can slightly boost invoke speed in exchange for text color
            helm-swoop-speed-or-color t
            ;; ;; Go to the opposite side of line from the end or beginning of line
            helm-swoop-move-to-line-cycle t
            ;; Optional face for line numbers
            ;; Face name is `helm-swoop-line-number-face`
            helm-swoop-use-line-number-face t))))

(defun bryan-racket ()
  (setq geiser-active-implementations '(racket))

  (defun racket-mode-custom-hook ()
    (bind-key "s-e" 'geiser-eval-definition scheme-mode-map))

  (require 'ac-geiser)
  (add-hook 'geiser-mode-hook 'ac-geiser-setup)
  (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
  (add-hook 'geiser-mode-hook 'racket-mode-custom-hook)

  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'geiser-repl-mode)))

(defun bryan-multiple-cursors ()
  ;; https://github.com/magnars/multiple-cursors.el
  (use-package multiple-cursors
    :ensure t
    :defer t
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C-<" . mc/mark-all-like-this))))

(defun bryan-clean-mode-line ()
  (defvar mode-line-cleaner-alist
    `((auto-complete-mode     . " α")
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
      (helm-mode              . "")
      (undo-tree-mode         . "")
      (auto-complete-mode     . "")
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

  (add-hook 'init-hook 'clean-mode-line)
  (add-hook 'after-change-major-mode-hook 'clean-mode-line))

(defun bryan-toggle-split-or-rotate-windows ()

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
                    'split-window-horizontally
                  'split-window-vertically)))
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

(defun bryan-smart-quotes ()

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

  (global-set-key (kbd "C-c y") 'yank-and-replace-smart-quotes))

(defun bryan-scala ()
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

  (add-hook 'sbt-mode-hook 'sbt-mode-custom-hook))

(defun bryan-magit ()
  (use-package magit
    :ensure t
    :config
    (progn
      (setq magit-last-seen-setup-instructions "1.4.0")
      (setq magit-status-buffer-switch-function 'switch-to-buffer)
      (setq magit-push-always-verify nil))))

(defun bryan-pandoc ()
  (use-package pandoc-mode
    :ensure t)

  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(defun bryan-ace-jump ()
  (use-package ace-jump-mode
    :ensure t)

  (progn
    (autoload
      'ace-jump-mode
      "ace-jump-mode"
      "Emacs quick move minor mode"
      t))
  (bind-key "C-c SPC" 'ace-jump-mode)

  (autoload
    'ace-jump-mode-pop-mark
    "ace-jump-mode"
    "Ace jump back:-)"
    t)
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
  (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

  ;; http://www.emacswiki.org/emacs/Evil#toc17

  ;; AceJump integration is now included in evil, this gist is only preserved for historical reasons.
  ;; Please use the provided integration (it's far more advanced)

  ;; AceJump is a nice addition to evil's standard motions.

  ;; The following definitions are necessary to define evil motions for ace-jump-mode (version 2).

  ;; ace-jump is actually a series of commands which makes handling by evil
  ;; difficult (and with some other things as well), using this macro we let it
  ;; appear as one.

  (defmacro evil-enclose-ace-jump (&rest body)
    `(let ((old-mark (mark))
           (ace-jump-mode-scope 'window))
       (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
       (remove-hook 'post-command-hook #'evil-visual-post-command t)
       (unwind-protect
           (progn
             ,@body
             (recursive-edit))
         (if (evil-visual-state-p)
             (progn
               (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
               (add-hook 'post-command-hook #'evil-visual-post-command nil t)
               (set-mark old-mark))
           (push-mark old-mark)))))

  (evil-define-motion evil-ace-jump-char-mode (count)
    :type exclusive
    (evil-enclose-ace-jump
     (ace-jump-mode 5)))

  (evil-define-motion evil-ace-jump-line-mode (count)
    :type line
    (evil-enclose-ace-jump
     (ace-jump-mode 9)))

  (evil-define-motion evil-ace-jump-word-mode (count)
    :type exclusive
    (evil-enclose-ace-jump
     (ace-jump-mode 1)))

  (evil-define-motion evil-ace-jump-char-to-mode (count)
    :type exclusive
    (evil-enclose-ace-jump
     (ace-jump-mode 5)
     (forward-char -1)))

  (add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

  ;; some proposals for binding:

  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
  (define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

  (define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode)      ; similar to f
  (define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
  (define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

  ;; different jumps for different visual modes
  (defadvice evil-visual-line (before spc-for-line-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

  (defadvice evil-visual-char (before spc-for-char-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

  (defadvice evil-visual-block (before spc-for-char-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)))

(defun bryan-hy ()
  (use-package hy-mode
    :ensure t)

  (add-hook 'hy-mode-hook 'paredit-mode))

(defun bryan-rcirc ()
  (setq rcirc-default-nick      "wolfcore"
        rcirc-default-user-name "wolfcore"
        rcirc-default-full-name "wolfcore"
        rcirc-prompt            "%n> "
        rcirc-omit-responses    '("JOIN" "PART" "QUIT" "NICK" "AWAY")
        rcirc-auto-authenticate-flag t
        rcirc-server-alist '(("chat.freenode.net"
                              :port 6697
                              :encryption tls
                              :channels ("#haskell"
                                         "#haskell-beginners"
                                         "#emacs"
                                         "#emacs-beginners"))))

  (rcirc-track-minor-mode 1)
  (set-face-foreground 'rcirc-prompt "#d7ff00")
  (set-face-background 'rcirc-prompt nil)

  (add-hook 'rcirc-mode-hook
            (lambda ()
              (set (make-local-variable 'scroll-conservatively)
                   8192)))

  (defun-rcirc-command reconnect (arg)
    "Reconnect the server process."
    (interactive "i")
    (if (buffer-live-p rcirc-server-buffer)
        (with-current-buffer rcirc-server-buffer
          (let ((reconnect-buffer (current-buffer))
                (server (or rcirc-server rcirc-default-server))
                (port (if (boundp 'rcirc-port) rcirc-port rcirc-default-port))
                (nick (or rcirc-nick rcirc-default-nick))
                channels)
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (equal reconnect-buffer rcirc-server-buffer)
                  (remove-hook 'change-major-mode-hook
                               'rcirc-change-major-mode-hook)
                  (let ((server-plist (cdr (assoc-string server rcirc-server-alist))))
                    (when server-plist
                      (setq channels (plist-get server-plist :channels))))
                  )))
            (if process (delete-process process))
            (rcirc-connect server port nick
                           nil
                           nil
                           channels)))))

  (defun rcirc-change-title (&rest dontcare)
    (if (string= rcirc-activity-string "[]")
        (setq frame-title-format "No IRC activity.")
      (setq frame-title-format rcirc-activity-string))
    (redisplay))
  (add-hook 'rcirc-update-activity-string-hook 'rcirc-change-title))

(defun bryan-w3m ()
  ;; http://www.emacswiki.org/emacs/emacs-w3m
  ;; http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
  (use-package w3m
    :ensure t
    :config
    (progn
      (setq browse-url-browser-function 'w3m-goto-url-new-session)
      (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
      (setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
      (defun reddit (reddit)
        "Opens the REDDIT in w3m-new-session"
        (interactive (list
                      (read-string "Enter the reddit (default: haskell): " nil nil "haskell" nil)))
        (browse-url (format "http://m.reddit.com/r/%s" reddit))))))

(defun bryan-rust ()
  (use-package rust-mode
    :ensure t)

  ;; (setq racer-rust-src-path "~/.emacs.d/lisp/racer/src/")
  ;; (setq racer-cmd "~/.emacs.d/lisp/racer/target/release/racer")
  ;; (add-to-list 'load-path "~/.emacs.d/lisp/racer/editors/emacs/")
  ;; (eval-after-load "rust-mode" '(require 'racer))

  ;; (defun rust-custom-hook ()
  ;;   (racer-activate)
  ;;   (local-set-key (kbd "M-.") #'racer-find-definition)
  ;;   (local-set-key (kbd "TAB") #'racer-complete-or-indent))

  ;; (add-hook 'rust-mode-hook 'rust-custom-hook)
  )

(defun bryan-elisp ()
  ;; Automatically load paredit when editing a lisp file
  ;; More at http://www.emacswiki.org/emacs/ParEdit
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; eldoc-mode shows documentation in the minibuffer when writing code
  ;; http://www.emacswiki.org/emacs/ElDoc
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

  (require 'elisp-slime-nav))

(defun bryan-clojure ()
  ;; Enable paredit for Clojure
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)

  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (add-hook 'clojure-mode-hook 'subword-mode)

  ;; A little more syntax highlighting
  (require 'clojure-mode-extra-font-locking)

  ;; syntax hilighting for midje
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq inferior-lisp-program "lein repl")
              (font-lock-add-keywords
               nil
               '(("(\\(facts?\\)"
                  (1 font-lock-keyword-face))
                 ("(\\(background?\\)"
                  (1 font-lock-keyword-face))))
              (define-clojure-indent (fact 1))
              (define-clojure-indent (facts 1))))

;;;;
  ;; Cider
;;;;

  ;; provides minibuffer documentation for the code you're typing into the repl
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  ;; enable paredit in your REPL
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; Use clojure mode for other extensions
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


  ;; key bindings
  ;; these help me out with the way I usually develop web apps
  (defun cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))

  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))

  (eval-after-load 'cider
    '(progn
       (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
       (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
       (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
       (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns))))

(defun bryan-ox-html ()
  (defun org-html-template (contents info)
    "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
    (concat
     (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
       (let* ((xml-declaration (plist-get info :html-xml-declaration))
              (decl (or (and (stringp xml-declaration) xml-declaration)
                        (cdr (assoc (plist-get info :html-extension)
                                    xml-declaration))
                        (cdr (assoc "html" xml-declaration))
                        "")))
         (when (not (or (not decl) (string= "" decl)))
           (format "%s\n"
                   (format decl
                           (or (and org-html-coding-system
                                    (fboundp 'coding-system-get)
                                    (coding-system-get org-html-coding-system 'mime-charset))
                               "iso-8859-1"))))))
     (org-html-doctype info)
     "\n"
     (concat "<html"
             (when (org-html-xhtml-p info)
               (format
                " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                (plist-get info :language) (plist-get info :language)))
             ">\n")
     "<head>\n"
     (org-html--build-meta-info info)
     (org-html--build-head info)
     (org-html--build-mathjax-config info)
     "</head>\n"
     "<body>\n"
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format (plist-get info :html-home/up-format)
                 (or link-up link-home)
                 (or link-home link-up))))
     ;; Preamble.
     (org-html--build-pre/postamble 'preamble info)
     ;; Document contents.
     (let ((div (assq 'content (plist-get info :html-divs))))
       (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (plist-get info :title))
             (subtitle (plist-get info :subtitle)))
         (when title
           (format
            (if (plist-get info :html-html5-fancy)
                "<header>\n<img id=\"shredder\" src=\"img/shredder.png\"><h1 id=\"myname\"><a href=\"/\">Bryan Garza</a></h1>
<p>Hi! Welcome to my blog on Haskell, Emacs, and hacking.<br>Read more <a href=\"about.html\">about me</a>, <a href=\"contact.html\">get in touch</a>, or find me on <i class=\"demo-icon icon-github-circled\"><a href=\"https://github.com/bryangarza\" >&#xe801;</a></i> and <i class=\"demo-icon icon-twitter\"><a href=\"https://twitter.com/bryangarza\">&#xe800;</a></i>.</p>\n</header>\n<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
              "\n<h1 class=\"title\">%s%s</h1>\n")
            (org-export-data title info)
            (if subtitle
                (format
                 (if (plist-get info :html-html5-fancy)
                     "<p class=\"subtitle\">%s</p>\n"
                   "\n<br>\n<span class=\"subtitle\">%s</span>\n")
                 (org-export-data subtitle info))
              "")))))
     contents
     (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
     ;; Postamble.
     (org-html--build-pre/postamble 'postamble info)
     ;; Closing document.
     "</body>\n</html>")))

(defun bryan-blog ()
  (setq org-publish-project-alist
        '(("blog"
           :base-directory "~/bryangarza.github.io/org/"
           :publishing-directory "~/bryangarza.github.io/"
           :recursive t

           :base-extension "org"
           :html-extension "html"

           :publishing-function (org-html-publish-to-html)

           :html-preamble nil
           :html-postamble "<footer>
<p>∴ <a href=\"about.html\">About</a>, <a href=\"contact.html\">Contact</a>, <a href=\"https://github.com/bryangarza\">GitHub</a>, <a href=\"https://twitter.com/bryangarza\">Twitter</a></p>
</footer>"

           :html-doctype "html5"
           :with-toc nil
           :with-timestamps t
           :section-numbers nil
           :html-head-include-default-style nil
           :html-head-include-scripts nil

           :html-head "<meta charset=\"utf-8\">
<link href=\"http://fonts.googleapis.com/css?family=Kaushan+Script\" rel=\"stylesheet\" type=\"text/css\" />
<meta name=\"viewport\" content=\"width=560\">
<link rel=\"stylesheet\" href=\"css/normalize.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/org.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/styles.css\" type=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/tables.css\" type=\"text/css\" />"

           :auto-sitemap nil)))

  ;; use <header>, <aside>, and other fancy tags
  (setq org-html-html5-fancy t))

(defun bryan-keybindings ()

  (setq mac-function-modifier 'hyper)
  (global-set-key [(super t)] nil)

  (defun pop-for-avy ()
    (interactive)
    (set-mark-command 4))

  (bind-keys*
   ("M-w"   . execute-extended-command)
   ("s-w"   . execute-extended-command)
   ("C-s-r" . zshell)
   ("s-r"   . eshell)
   ("s-s"   . save-buffer)
   ("s-R"   . prelude-rename-buffer-and-file)
   ("s-D"   . prelude-delete-file-and-buffer)
   ("s-i"   . magit-status)
   ("s-o"   . other-window)
   ("s-O"   . other-frame)
   ("s-e"   . eval-defun)
   ("s-n"   . next-buffer)
   ("s-p"   . previous-buffer)
   ("s-P"   . ns-print-buffer)
   ("s-h"   . windmove-left)
   ("s-j"   . windmove-down)
   ("s-k"   . windmove-up)
   ("s-l"   . windmove-right)
   ("s-L"   . goto-line)
   ("s-l"   . windmove-right)
   ("s-m"   . org-publish-current-project)
   ("s-q"   . previous-history-element)
   ("s-d"   . next-history-element)
   ("s-K"   . kill-this-buffer)
   ("C-x 2" . split-vert-and-switch)
   ("C-x 3" . split-horiz-and-switch)
   ("s-0"   . delete-window)
   ("s-1"   . delete-other-windows)
   ("s-2"   . split-vert-and-switch)
   ("s-3"   . split-horiz-and-switch)
   ("C-c w" . bryan-window-stuff-hydra/body)
   ("C-c c" . bryan-multiple-cursors-hydra/body)
   ("C-c o" . bryan-org-hydra/body)
   ("C-c a" . bryan-avy-hydra/body)
   ("C-c m" . bryan-hydra-hydra/body)
   ("C-c r" . evil-ranger)
   ("C-;"   . pop-for-avy)
   ("s-A"   . evil-copy-from-above)
   ("s-u"   . revert-buffer))

  (bind-keys :map evil-motion-state-map ("C-e" . move-end-of-line))
  (bind-keys :map evil-insert-state-map ("C-e" . move-end-of-line))
  (bind-keys :map evil-visual-state-map ("C-e" . move-end-of-line))
  (bind-keys :map evil-normal-state-map ("C-e" . move-end-of-line))
  ;; ("C-e" . evil-copy-from-below)

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)

  (bind-key "M-)" 'paredit-wrap-round-from-behind evil-motion-state-map)
  (bind-key "M-)" 'paredit-wrap-round-from-behind evil-insert-state-map)

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)
    (evil-leader/set-key
      "s" 'elisp-slime-nav-find-elisp-thing-at-point
      "S" 'pop-tag-mark
      "d" 'elisp-slime-nav-describe-elisp-thing-at-point))

  (add-hook 'term-mode-hook (lambda ()
                              (define-key term-raw-map (kbd "s-v") 'term-paste)))

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

(defun bryan-eval-fns ()
  (bryan-company)
  (bryan-paren)
  (bryan-themes)
  (bryan-org)
  (bryan-general)
  (bryan-interface)
  (bryan-hydra)
  (bryan-paredit)
  (bryan-hooks)
  (bryan-evil)
  (bryan-avy)
  (bryan-cider)
  (bryan-auto-complete)
  (bryan-frontend)
  (bryan-markdown)
  (bryan-json)
  (bryan-term)
  (bryan-helm)
  (bryan-better-splits)
  (bryan-rename-or-delete-buffer-and-file)
  (bryan-minibuffer-keyboard-quit)
  (bryan-util)
  (bryan-flycheck)
  (bryan-c)
  (bryan-ocaml)
  (bryan-haskell)
  (bryan-elm)
  ;; (bryan-circe)
  (bryan-helm-swoop)
  (bryan-racket)
  (bryan-multiple-cursors)
  (bryan-clean-mode-line)
  (bryan-toggle-split-or-rotate-windows)
  (bryan-smart-quotes)
  (bryan-scala)
  (bryan-magit)
  (bryan-pandoc)
  ;; (bryan-ace-jump)
  (bryan-hy)
  (bryan-rcirc)
  ;; (bryan-w3m)
  (bryan-rust)
  (bryan-elisp)
  (bryan-clojure)
  (bryan-ox-html)
  (bryan-blog)
  (bryan-keybindings))

(bryan-eval-fns)

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
;; (add-hook 'text-mode-hook 'remove-dos-eol)

(add-to-list 'evil-insert-state-modes 'haskell-interactive-mode)
(add-to-list 'evil-insert-state-modes 'twittering-edit-mode)

;; http://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :config
  (progn
    (global-set-key [f8] 'neotree-toggle)
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

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
