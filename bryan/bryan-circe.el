;; circe.el

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

(provide 'bryan-circe)
