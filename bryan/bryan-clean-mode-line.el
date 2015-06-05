;; clean-mode-line.el

(defvar mode-line-cleaner-alist
  `((auto-complete-mode     . " α")
    (paredit-mode           . " π")
    (inf-haskell-mode       . " λinf")
    (hi2-mode               . " hi2")
    ;; Major modes
    (lisp-interaction-mode  . "λeval")
    (lisp-mode              . "(())")
    (scheme-mode            . "λscm")
    (racket-mode            . "λrkt")
    (clojure-mode           . "λclj")
    (emacs-lisp-mode        . "λel")
    (common-lisp-mode       . "λcl")
    (haskell-mode           . "λ")
    (literate-haskell-mode  . "λlit")
    (inferior-haskell-mode  . "λinf")
    (tuareg-mode            . "λOCaml")
    (python-mode            . "py")
    (doc-view-mode           . "dv")
    ;; hidden
    (helm-mode              . "")
    (undo-tree-mode         . "")
    (auto-complete-mode     . "")
    (magit-auto-revert-mode . "")
    (eldoc-mode             . "")
    (elisp-slime-nav-mode   . ""))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'init-hook 'clean-mode-line)
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(provide 'bryan-clean-mode-line)
