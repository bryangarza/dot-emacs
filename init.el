(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(evil paredit
		      evil-paredit evil-surround
		      rainbow-delimiters
		      ;; http://www.emacswiki.org/emacs/PareditCheatsheet
		      ;; http://mumble.net/~campbell/emacs/paredit.html
		      smartparens
		      clojure-mode-extra-font-locking
		      cider
		      magit
		      linum-relative))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(menu-bar-mode -1)
(global-linum-mode 1) ; display line numbers
(column-number-mode 1) ; display column and row of cursor in mode-line

(add-hook 'text-mode-hook
	  (lambda ()
            ;; ask to turn on hard line wrapping
            (when (y-or-n-p "Hard wrap text?")
              (turn-on-auto-fill))))

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

(require 'navigate)
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
