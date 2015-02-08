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
                      json-mode))

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
            (turn-off-auto-fill)))

(require 'evil)
(evil-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(setq electric-indent-mode nil)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

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
 '(linum-relative-current-face ((t (:inherit linum :background "#292929" :foreground "White" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "White"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "Yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "Green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "Orange"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "Purple"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "Yellow"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "Brown"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "Magenta")))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
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

; Tabs are evil
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil
                    :family "Menlo" :height 140 :weight 'normal)

(require 'json-mode)
(add-hook 'json-mode-hook
          '(lambda ()
             (setq c-basic-offset 2)
             (setq js-indent-level 2)
             (setq json-reformat:indent-width 4)))

(global-set-key [(control c) r] 'revert-buffer)
