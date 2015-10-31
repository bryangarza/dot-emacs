;; rust.el

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

(provide 'bryan-rust)
