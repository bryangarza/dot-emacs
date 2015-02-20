(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil
                      paredit
                      evil-paredit
                      evil-surround
                      rainbow-delimiters
                      ;; http://www.emacswiki.org/emacs/PareditCheatsheet
                      ;; http://mumble.net/~campbell/emacs/paredit.html
                      smartparens
                      clojure-mode-extra-font-locking
                      cider
                      magit
                      linum-relative
                      company
                      json-mode
                      exec-path-from-shell
                      flycheck
                      haskell-mode))

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
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(setq electric-indent-mode nil)

;; change mode-line color by evil state
;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                    (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let ((color (cond ((minibufferp) default-color)
;;                                  ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;;                                  ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                                  ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                                  (t default-color))))
;;                 (set-face-background 'mode-line (car color))
;;                 (set-face-foreground 'mode-line (cdr color))))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/customizations")

(load-theme 'noctilux t)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'linum-relative)
(setq linum-relative-current-symbol "")

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(load "elisp-editing.el")
(load "setup-clojure.el")

;; Stop littering everywhere w save files, put them somewhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

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

;; Use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; fix the terminal
(setq system-uses-terminfo nil)

(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((web-mode-css-indent-offset . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((t (:inherit error :background "#202020"))))
 '(compilation-info ((t (:background "#202020" :foreground "#aaffaa" :inverse-video nil :underline nil :slant normal :weight bold))))
 '(fringe ((t (:background "#202020" :foreground "#5f5f5f"))))
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

(setq mac-function-modifier 'hyper)

(global-set-key [(meta w)] 'execute-extended-command)
(global-set-key [(super w)] 'execute-extended-command)
(global-set-key [(super r)] 'revert-buffer)
(global-set-key [(super i)] 'magit-status)
(global-set-key [(super f)] 'ido-find-file)
(global-set-key [(super b)] 'ido-switch-buffer)
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
(global-set-key [(super m)] 'recentf-open-files)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key [(super shift f)] 'recentf-ido-find-file)

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


(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

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
  (flycheck-mode)
  (paredit-mode 1))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-custom-hook)
