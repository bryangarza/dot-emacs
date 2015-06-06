;; interface.el

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode global-hl-line-mode))
  (when (fboundp mode) (funcall mode -1)))

(column-number-mode t)
(blink-cursor-mode +1)
(display-time)
(display-battery-mode)

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)
;; (set-face-attribute 'default nil
;;                     :family "Droid Sans Mono Slashed" :height 140 :weight 'normal)
(set-face-attribute 'default nil
                    :family "Monaco" :height 150 :weight 'normal)

(provide 'bryan-interface)
