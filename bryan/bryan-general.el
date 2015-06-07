;; general.el

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
(require 'bind-key)

(provide 'bryan-general)
