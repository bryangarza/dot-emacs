;; themes.el

(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/color-theme-ujelly")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-theme-darktooth")
;; (use-package tao-theme
;;   :ensure t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe-theme.el/")
(add-to-list 'load-path "~/.emacs.d/themes/moe-theme.el/")
;; so kawaii~  ✿◕ ‿ ◕✿

;; USUAL SETUP ;;;;;;;;;;;;
(require 'moe-theme)
(setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
(setq moe-theme-highlight-buffer-id nil)
(moe-theme-set-color 'purple)
;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
(moe-light)
;; (moe-dark)
(set-background-color "#ffffff")

;; no underlined text! include `:weight 'normal` to get rid of bold
;; (but who would wanna do that?)
;; another example: (set-face-bold-p 'bold nil)

;; (mapc
;;   (lambda (face)
;;     (set-face-attribute face nil :underline nil))
;;   (face-list))

;; (set-frame-parameter nil 'background-mode 'dark)
;; (load-theme 'cyberpunk t)
;; (load-theme 'darktooth t)
;; (load-theme 'dichromacy t)
;; (load-theme 'tao-yang)
;; (load-theme 'leuven)

;; (setq rainbow-delimiters-outermost-only-face-count 1)
(setq rainbow-delimiters-max-face-count 2)

(provide 'bryan-themes)
