(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; http://www.emacswiki.org/emacs/PareditCheatsheet
;; http://mumble.net/~campbell/emacs/paredit.html
(defvar my-packages '(evil
                      evil-paredit
                      evil-surround
                      paredit
                      smartparens
                      rainbow-delimiters
                      rainbow-mode
                      clojure-mode-extra-font-locking
                      cider
                      magit
                      json-mode
                      exec-path-from-shell
                      flycheck
                      circe
                      debbugs
                      ac-helm
                      geiser
                      ac-geiser
                      multiple-cursors
                      expand-region
                      auctex))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/customizations")

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; (add-to-list 'load-path "~/.emacs.d/lisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/lisp/haskell-mode/")

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/color-theme-ujelly")
;; (set-frame-parameter nil 'background-mode 'dark)
(load-theme 'cyberpunk t)
;; (load-theme 'solarized t)

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

(add-hook 'prog-mode-hook 'linum-mode)

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
  :init
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

(use-package paren
  :ensure t
  :init
  (progn
    (show-paren-mode 1)
    (set-face-background 'show-paren-match (face-background 'default))
    (set-face-foreground 'show-paren-match "#def")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold :underline t)
    (setq show-paren-delay 0)))

;; Cider settings
;; Enable `eldoc` in Clojure buffers
(add-hook 'cider-mode-hool #'eldoc-mode)
;; Log communication with the nREPL server
(setq nrepl-log-messages t)
;; Hide *nrepl-connection* and *nrepl-server* from some buffer switching
;; comands like `switch-to-buffer` (C-x b)
(setq nrepl-hide-special-buffers t)
;; Enable paredit in the REPL buffer
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(use-package auto-complete
  :ensure t
  :defer t
  :config
  (progn
    (require 'auto-complete-config)
    (auto-complete-mode t)))

(use-package web-mode
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.scss\\'"  . web-mode)))

(use-package markdown-mode
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

(add-hook 'markdown-mode-hook 'pandoc-mode)

;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (setq js2-strict-missing-semi-warning nil)

(use-package json-mode
  :config
  (progn
    (setq c-basic-offset 2)
    (setq js-indent-level 2)
    (setq json-reformat:indent-width 4)))

;; http://emacs-helm.github.io/helm/
;; http://tuhdo.github.io/helm-intro.html
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq
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

(use-package helm-eshell
  :defer t
  :config
  (progn
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (bind-key "C-c C-l" 'helm-eshell-history eshell-mode-map)))))

(use-package ac-helm
  :defer t
  :config (bind-key "C-:" 'ac-complete-with-helm ac-complete-mode-map)
  :bind ("C-:" . ac-complete-with-helm))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

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

(setq mac-function-modifier 'hyper)
(global-set-key [(super t)] nil)

(bind-keys*
 ("M-w"   . execute-extended-command)
 ("s-w"   . execute-extended-command)
 ("s-r"   . revert-buffer)
 ("s-i"   . magit-status)
 ("s-o"   . other-window)
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
 ("s-q"   . previous-history-element)
 ("s-d"   . next-history-element)
 ("s-K"   . kill-this-buffer)
 ("C-x 2" . split-vert-and-switch)
 ("C-x 3" . split-horiz-and-switch)
 ("s-0"   . delete-window)
 ("s-1"   . delete-other-windows)
 ("s-2"   . split-vert-and-switch)
 ("s-3"   . split-horiz-and-switch))

(bind-keys :map evil-motion-state-map
           ("C-e" . move-end-of-line)
           ("C-e" . move-end-of-line))
           ;; ("C-e" . evil-copy-from-below)

(use-package sws-mode
  :mode "\\.styl$")

(use-package jade-mode
  :mode "\\.jade$")

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
;; https://github.com/howardabrams/dot-files/blob/master/emacs-irc.org
;; https://github.com/jorgenschaefer/circe/wiki/Configuration
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
         lui-time-stamp-format "%H:%M")))))

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
          helm-swoop-use-line-number-face t)))

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

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

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
    (git-gutter-mode        . ""))
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
(add-hook 'after-change-major-mode-hook 'clean-mode-line)
(add-hook 'window-startup-hook 'toggle-frame-maximized)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(bind-key "M-)" 'paredit-wrap-round-from-behind evil-motion-state-map)
(bind-key "M-)" 'paredit-wrap-round-from-behind evil-insert-state-map)

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

(bind-key "C-c w s" 'toggle-window-split)

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
             (setq i (1+ i)))))))

(bind-key "C-c w r" 'rotate-windows)

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode)
  (evil-leader/set-key
   "s" 'elisp-slime-nav-find-elisp-thing-at-point
   "S" 'pop-tag-mark
   "d" 'elisp-slime-nav-describe-elisp-thing-at-point))

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

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

(use-package pandoc-mode
  :ensure t)

(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(use-package ace-jump-mode
  :ensure t)

(progn
  (autoload
    'ace-jump-mode
    "ace-jump-mode"
    "Emacs quick move minor mode"
    t))

(bind-key "C-c SPC" 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(use-package git-gutter-fringe
  :ensure t
  :config
  (progn
    (require 'git-gutter-fringe)
    (set-face-foreground 'git-gutter-fr:modified "lawn green")
    (set-face-foreground 'git-gutter-fr:added    "deep sky blue")
    (set-face-foreground 'git-gutter-fr:deleted  "red"))
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)

         ;; Jump to next/previous hunk
         ("C-x p"   . git-gutter:previous-hunk)
         ("C-x n"   . git-gutter:next-hunk)

         ;; Stage current hunk
         ("C-x v s" . git-gutter:stage-hunk)

         ;; Revert current hunk
         ("C-x v r" . git-gutter:revert-hunk)))

;; Org-present, keys are:

;; left/right for movement
;; C-c C-= for large txt
;; C-c C-- for small text
;; C-c C-q for quit (which will return you back to vanilla org-mode)
;; C-c < and C-c > to jump to first/last slide

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

(setq org-src-fontify-natively t)

;; Zoom
;; You can use `C-x C-+’ and ‘C-x C--’ (‘text-scale-adjust’) to increase
;; or decrease the buffer text size (`C-+’ or ‘C--’ to repeat). To
;; restore the default (global) face height, type ‘C-x C-0’. ‘S-mouse-1’
;; pops up a menu where you can choose these same actions.

(use-package hy-mode
  :ensure t)

(add-hook 'hy-mode-hook 'paredit-mode)

(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#haskell" "#emacs"))))

(setq rcirc-authinfo '(("freenode" nickserv ,freenode-username ,freenode-password)))

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


;; W3M: Otherwise known as "Bryan trying to be a hipster by using a text-based web browser"
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
       (browse-url (format "http://m.reddit.com/r/%s" reddit)))))
