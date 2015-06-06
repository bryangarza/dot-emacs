;; evil.el

(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")))

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config (setq evil-move-cursor-back nil))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package evil-anzu
  :ensure t
  :config
  (progn
    (with-eval-after-load 'evil
      (require 'evil-anzu))))

(provide 'bryan-evil)
