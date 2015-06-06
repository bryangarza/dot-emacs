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

;; ლ(ಠ益ಠ)ლ ¡porque!
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'bryan-general)
