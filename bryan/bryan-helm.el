;; helm.el

(use-package helm
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq
     helm-scroll-amount                    8
     helm-split-window-in-side-p           t
     helm-move-to-line-cycle-in-source     t
     helm-ff-search-library-in-sexp        t
     helm-ff-file-name-history-use-recentf t
     helm-M-x-fuzzy-match                  t
     helm-quick-update                     t
     helm-bookmark-show-location           t
     helm-buffers-fuzzy-matching           t
     helm-apropos-fuzzy-match              t
     helm-recentf-fuzzy-match              t
     helm-locate-fuzzy-match               t
     helm-file-cache-fuzzy-match           t
     helm-semantic-fuzzy-match             t
     helm-imenu-fuzzy-match                t
     helm-lisp-fuzzy-completion            t)
    (helm-mode)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    (ido-mode -1) ; just in case
    (helm-autoresize-mode t)
    (global-unset-key (kbd "C-x c"))
    (bind-keys :map helm-map
               ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
               ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
               ("C-z"   . helm-select-action))            ; list actions using C-z
    (bind-key "C-c C-l" 'helm-comint-input-ring shell-mode-map)
    (bind-key "C-c C-l" 'helm-minibuffer-history minibuffer-local-map))
  :bind (("C-c h"     . helm-command-prefix)
         ("M-x"       . helm-M-x)
         ("s-m"       . helm-man-woman)
         ("M-y"       . helm-show-kill-ring)
         ("C-x b"     . helm-mini)
         ("C-x C-f"   . helm-find-files)
         ("s-f"       . helm-find-files)
         ("s-b"       . helm-mini)
         ("C-h SPC"   . helm-all-mark-rings)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-c h o"   . helm-occur)
         ("s-F"       . helm-occur)))

;; (require 'helm-eshell)
;; (add-hook 'eshell-mode-hook
;;               #'(lambda ()
;;                   (bind-key "C-c C-l" 'helm-eshell-history eshell-mode-map)))

;; (use-package ac-helm
;;   :defer t
;;   :config (bind-key "C-:" 'ac-complete-with-helm ac-complete-mode-map)
;;   :bind ("C-:" . ac-complete-with-helm))

;; (use-package helm-descbinds
;;   :defer t
;;   :bind (("C-h b" . helm-descbinds)
;;          ("C-h w" . helm-descbinds)))

(provide 'bryan-helm)
