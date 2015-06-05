;; json.el

(use-package json-mode
  :config
  (progn
    (setq c-basic-offset 2)
    (setq js-indent-level 2)
    (setq json-reformat:indent-width 4)))

(provide 'bryan-json)
