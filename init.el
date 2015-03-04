(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; http://www.emacswiki.org/emacs/PareditCheatsheet
;; http://mumble.net/~campbell/emacs/paredit.html
(defvar my-packages '(evil paredit evil-paredit evil-surround rainbow-delimiters
                           smartparens clojure-mode-extra-font-locking cider
                           magit linum-relative company json-mode
                           exec-path-from-shell flycheck haskell-mode circe
                           debbugs ac-helm geiser ac-geiser multiple-cursors))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode -1)
(global-linum-mode 1) ; display line numbers
(column-number-mode 1) ; display column and row of cursor in mode-line
(show-paren-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook
          (lambda ()
            ;; ask to turn on hard line wrapping
            ;; (when (y-or-n-p "Hard wrap text?")
            ;;   (turn-on-auto-fill))))
            (turn-on-auto-fill)))

(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(add-hook 'paredit-mode-hook 'evil-paredit-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(setq electric-indent-mode nil)
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/customizations")
(load-theme 'noctilux t)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(load "elisp-editing.el")
(load "setup-clojure.el")

;; Stop littering everywhere w save files, put them somewhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(setq scheme-program-name "/usr/local/bin/mit-scheme")

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold
                    :underline t)
(setq show-paren-delay 0)

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

(require 'auto-complete)
(require 'auto-complete-config)
(auto-complete-mode t)

;; fix the terminal
(setq system-uses-terminfo nil)

(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-fill-mode t)
 '(package-selected-packages
   (quote
    (multiple-cursors ac-helm ac-geiser auto-complete geiser smartparens rainbow-delimiters nyan-mode magit linum-relative json-mode js2-mode helm-descbinds haskell-mode flycheck exec-path-from-shell evil-surround evil-paredit diminish debbugs company clojure-mode-extra-font-locking circe cider)))
 '(safe-local-variable-values (quote ((web-mode-css-indent-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((t (:inherit error :background "#202020"))))
 '(compilation-info ((t (:background "#202020" :foreground "#aaffaa" :inverse-video nil :underline nil :slant normal :weight bold))))
 '(erc-input-face ((t (:foreground "White"))))
 '(erc-notice-face ((t (:foreground "#505050" :inverse-video nil :underline nil :slant normal :weight normal))))
 '(fringe ((t (:background "#202020" :foreground "#5f5f5f"))))
 '(helm-candidate-number ((t (:background "Yellow" :foreground "#202020"))))
 '(linum ((t (:background "#202020" :foreground "#5f5f5f"))))
 '(linum-relative-current-face ((t (:inherit linum :background "#202020" :foreground "White" :weight bold))))
 '(mode-line ((t (:background "Orange" :foreground "#202020" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(mode-line-inactive ((t (:background "gray50" :foreground "#202020" :inverse-video t :box nil :underline nil :slant normal :weight normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "White"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "Yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "Green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "Orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "Purple"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "Yellow"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "Brown"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "Magenta"))))
 '(vertical-border ((t (:foreground "#202020")))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'custom-web-mode-hook)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (setq js2-strict-missing-semi-warning nil)

; Tabs are evil
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil
                    :family "Menlo" :height 130 :weight 'normal)

(require 'json-mode)
(add-hook 'json-mode-hook
          '(lambda ()
             (setq c-basic-offset 2)
             (setq js-indent-level 2)
             (setq json-reformat:indent-width 4)))

;; Helm ^____^
;; http://emacs-helm.github.io/helm/
;; http://tuhdo.github.io/helm-intro.html

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key [(super m)] 'helm-man-woman)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key [(super f)] 'helm-find-files)
(global-set-key [(super b)] 'helm-mini)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

(helm-mode 1)
(ido-mode -1) ; just in case
(helm-autoresize-mode t)

;; getting errors (when scheme file was opened)
(semantic-mode 0)

(setq helm-M-x-fuzzy-match t)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-apropos-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-file-cache-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key [(super shift f)] 'helm-occur)

(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; Enter a prefix key and C-h after it. You will see a list of
;; bindings using Helm interface for narrowing.
(require 'helm-descbinds)
(helm-descbinds-mode)

(require 'ac-helm)  ;; Not necessary if using ELPA package
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

(setq mac-function-modifier 'hyper)

(global-set-key [(meta w)] 'execute-extended-command)
(global-set-key [(super w)] 'execute-extended-command)
(global-set-key [(super r)] 'revert-buffer)
(global-set-key [(super i)] 'magit-status)
(global-set-key [(super o)] 'other-window)
(global-set-key [(super e)] 'eval-defun)
(global-set-key [(super n)] 'next-buffer)
(global-set-key [(super p)] 'previous-buffer)
(global-set-key [(super shift p)] 'ns-print-buffer)
(global-set-key [(super t)] nil)

(global-set-key [(super h)] 'windmove-left)
(global-set-key [(super j)] 'windmove-down)
(global-set-key [(super k)] 'windmove-up)
(global-set-key [(super l)] 'windmove-right)
(global-set-key [(super shift l)] 'goto-line)
(global-set-key [(super l)] 'windmove-right)

(global-set-key [(super q)] 'previous-history-element)
(global-set-key [(super d)] 'next-history-element)
(global-set-key [(super shift k)] 'kill-this-buffer)

(defun split-vert-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-horiz-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(global-set-key "\C-x2" 'split-vert-and-switch)
(global-set-key "\C-x3" 'split-horiz-and-switch)

(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-vert-and-switch)
(global-set-key (kbd "s-3") 'split-horiz-and-switch)

(setq ring-bell-function 'ignore)
(setq-default show-trailing-whitespace t)

(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(winner-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 25)

(setq evil-move-cursor-back nil)

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

(require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jshint))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; IDK how I feel about this
(global-hl-line-mode 1)

(defun paredit-nonlisp-hook ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(defun c-mode-custom-hook ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4))

(add-hook 'js-mode-hook 'paredit-nonlisp-hook)
(add-hook 'c-mode-common-hook 'paredit-nonlisp-hook)
(add-hook 'c-mode-common-hook 'c-mode-custom-hook)

(setq scroll-step 1 scroll-conservatively 10000)

;;; Ocaml setup
(load "/Users/bryangarza/.emacs.d/lisp/tuareg/tuareg-site-file.el")

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

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

(defun haskell-custom-hook ()
  ;; Getting tired of these 2 sometimes
  ;; (flycheck-mode)
  ;; (paredit-mode 1)
  (turn-on-haskell-indentation)
  (inf-haskell-mode))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-custom-hook)
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; For later: https://wiki.haskell.org/Emacs/Indentation#Aligning_code
(global-set-key (kbd "C-x a r") 'align-regexp)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; ლ(ಠ益ಠ)ლ ¡porque!
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(load-file "~/.private.el")

(setq circe-network-options
      `(("Freenode"
         :tls t
         :service 6697
         :nick "wolfcore"
         :channels ("#haskell" "#emacs")
         :nickserv-password ,freenode-password)))

(add-hook 'circe-server-mode-hook
          '(lambda ()
             (setq-default show-trailing-whitespace nil)))

;; https://github.com/howardabrams/dot-files/blob/master/emacs-irc.org
;; https://github.com/jorgenschaefer/circe/wiki/Configuration

(setq circe-reduce-lurker-spam t)
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M")

(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  (setq right-margin-width 5))

(setq circe-default-part-message "bye!")
(setq circe-default-quit-message "bye!")

(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
(define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-horizontally)
;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color nil)
;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)
;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; (setq geiser-active-implementations '(racket))

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
