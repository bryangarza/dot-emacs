;; interface.el

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode -1)
(column-number-mode t)
(global-hl-line-mode -1)
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
