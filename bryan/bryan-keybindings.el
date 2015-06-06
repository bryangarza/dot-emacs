;; keybindings.el

(setq mac-function-modifier 'hyper)
(global-set-key [(super t)] nil)

(bind-keys*
 ("M-w"     . execute-extended-command)
 ("s-w"     . execute-extended-command)
 ("s-r"     . revert-buffer)
 ("s-R"     . prelude-rename-buffer-or-file)
 ("s-D"     . prelude-delete-file-and-buffer)
 ("s-i"     . magit-status)
 ("s-o"     . other-window)
 ("s-e"     . eval-defun)
 ("s-n"     . next-buffer)
 ("s-p"     . previous-buffer)
 ("s-P"     . ns-print-buffer)
 ("s-h"     . windmove-left)
 ("s-j"     . windmove-down)
 ("s-k"     . windmove-up)
 ("s-l"     . windmove-right)
 ("s-L"     . goto-line)
 ("s-l"     . windmove-right)
 ("s-q"     . previous-history-element)
 ("s-d"     . next-history-element)
 ("s-K"     . kill-this-buffer)
 ("C-x 2"   . split-vert-and-switch)
 ("C-x 3"   . split-horiz-and-switch)
 ("s-0"     . delete-window)
 ("s-1"     . delete-other-windows)
 ("s-2"     . split-vert-and-switch)
 ("s-3"     . split-horiz-and-switch)
 ("C-c w s" . toggle-window-split)
 ("C-c w r" . rotate-windows))

(bind-keys :map evil-motion-state-map
           ("C-e" . move-end-of-line))
;; ("C-e" . evil-copy-from-below)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(bind-key "M-)" 'paredit-wrap-round-from-behind evil-motion-state-map)
(bind-key "M-)" 'paredit-wrap-round-from-behind evil-insert-state-map)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode)
  (evil-leader/set-key
    "s" 'elisp-slime-nav-find-elisp-thing-at-point
    "S" 'pop-tag-mark
    "d" 'elisp-slime-nav-describe-elisp-thing-at-point))

(provide 'bryan-keybindings)
