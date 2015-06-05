;; cider.el

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

(provide 'bryan-cider)
