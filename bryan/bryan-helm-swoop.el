;; helm-swoop.el

(use-package helm-swoop
  :defer t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
    (bind-key "M-i" 'helm-swoop-from-evil-search evil-motion-state-map)
    (bind-keys :map helm-swoop-map
               ("M-i" . helm-multi-swoop-all-from-helm-swoop)
               ("C-r" . helm-previous-line)
               ("C-s" . helm-next-line))
    (bind-keys :map helm-multi-swoop-map
               ("C-r" . helm-previous-line)
               ("C-s" . helm-next-line))
          ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t
          ;; If this value is t, split window inside the current window
          helm-swoop-split-with-multiple-windows nil
          ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
          helm-swoop-split-direction 'split-window-horizontally
          ;; If nil, you can slightly boost invoke speed in exchange for text color
          helm-swoop-speed-or-color t
          ;; ;; Go to the opposite side of line from the end or beginning of line
          helm-swoop-move-to-line-cycle t
          ;; Optional face for line numbers
          ;; Face name is `helm-swoop-line-number-face`
          helm-swoop-use-line-number-face t)))

(provide 'bryan-helm-swoop)
