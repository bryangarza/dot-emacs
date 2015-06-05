;; keybindings.el

(setq mac-function-modifier 'hyper)
(global-set-key [(super t)] nil)

(bind-keys*
 ("M-w"   . execute-extended-command)
 ("s-w"   . execute-extended-command)
 ("s-r"   . revert-buffer)
 ("s-R"   . prelude-rename-buffer-or-file)
 ("s-D"   . prelude-delete-file-and-buffer)
 ("s-i"   . magit-status)
 ("s-o"   . other-window)
 ("s-e"   . eval-defun)
 ("s-n"   . next-buffer)
 ("s-p"   . previous-buffer)
 ("s-P"   . ns-print-buffer)
 ("s-h"   . windmove-left)
 ("s-j"   . windmove-down)
 ("s-k"   . windmove-up)
 ("s-l"   . windmove-right)
 ("s-L"   . goto-line)
 ("s-l"   . windmove-right)
 ("s-q"   . previous-history-element)
 ("s-d"   . next-history-element)
 ("s-K"   . kill-this-buffer)
 ("C-x 2" . split-vert-and-switch)
 ("C-x 3" . split-horiz-and-switch)
 ("s-0"   . delete-window)
 ("s-1"   . delete-other-windows)
 ("s-2"   . split-vert-and-switch)
 ("s-3"   . split-horiz-and-switch))


(bind-keys :map evil-motion-state-map
           ("C-e" . move-end-of-line))
           ;; ("C-e" . evil-copy-from-below)

(provide 'bryan-keybindings.el)
