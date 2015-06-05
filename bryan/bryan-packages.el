;; packages.el

(defvar bryan/packages
  '(evil
    evil-paredit
    evil-surround
    paredit
    smartparens
    rainbow-delimiters
    rainbow-mode
    clojure-mode-extra-font-locking
    cider
    magit
    json-mode
    exec-path-from-shell
    flycheck
    circe
    debbugs
    geiser
    ac-geiser
    multiple-cursors
    expand-region
    auctex)
  "Default packages")

(defun bryan/packages-installed-p ()
  (loop for pkg in bryan/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(provide 'bryan-packages)
