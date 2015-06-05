;; ocaml.el

(load "/Users/bryangarza/.emacs.d/lisp/tuareg/tuareg-site-file.el")

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(use-package merlin
  :init
  (progn
    ;; Enable auto-complete
    (setq merlin-use-auto-complete-mode 'easy)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;; Start merlin on OCaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

(add-to-list 'load-path "/Users/bryangarza/.opam/system/share/emacs/site-lisp")

(require 'ocp-indent)

;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;; Update the emacs load path
(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)

(defun ocaml-custom-hook ()
  (paredit-mode 1))

;; (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;; (add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'ocaml-custom-hook)

(provide 'bryan-ocaml)
