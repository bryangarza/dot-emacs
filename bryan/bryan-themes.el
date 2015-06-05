;; themes.el

(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/color-theme-ujelly")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe-theme.el/")
(add-to-list 'load-path "~/.emacs.d/themes/moe-theme.el/")
;; so kawaii~  ✿◕ ‿ ◕✿
(require 'moe-theme)
;; (moe-light)
(setq moe-theme-highlight-buffer-id nil)
(moe-light)
(moe-theme-set-color 'green)

;; no underlined text! include `:weight 'normal` to get rid of bold
;; (but who would wanna do that?)
;; another example: (set-face-bold-p 'bold nil)
(mapc
  (lambda (face)
    (set-face-attribute face nil :underline nil))
  (face-list))
;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)

;; (set-frame-parameter nil 'background-mode 'dark)
;; (load-theme 'cyberpunk t)

(provide 'bryan-themes)
