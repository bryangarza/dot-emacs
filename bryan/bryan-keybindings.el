;; keybindings.el

(setq mac-function-modifier 'hyper)
(global-set-key [(super t)] nil)

(defun pop-for-avy ()
  (interactive)
  (set-mark-command 4))

(bind-keys*
 ("M-w"     . execute-extended-command)
 ("s-w"     . execute-extended-command)
 ("s-r"     . zshell)
 ("s-R"     . prelude-rename-buffer-and-file)
 ("s-D"     . prelude-delete-file-and-buffer)
 ("s-i"     . magit-status)
 ("s-o"     . other-window)
 ("s-O"     . other-frame)
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
 ("s-m"     . org-publish-current-project)
 ("s-q"     . previous-history-element)
 ("s-d"     . next-history-element)
 ("s-K"     . kill-this-buffer)
 ("C-x 2"   . split-vert-and-switch)
 ("C-x 3"   . split-horiz-and-switch)
 ("s-0"     . delete-window)
 ("s-1"     . delete-other-windows)
 ("s-2"     . split-vert-and-switch)
 ("s-3"     . split-horiz-and-switch)
 ("C-c w"   . bryan-window-stuff-hydra/body)
 ("C-c c"   . bryan-multiple-cursors-hydra/body)
 ("C-c o"   . bryan-org-hydra/body)
 ("C-c a"   . bryan-avy-hydra/body)
 ("C-c m"   . bryan-hydra-hydra/body)
 ("C-c r"   . evil-ranger)
 ("C-;"     . pop-for-avy)
 ("s-A"     . evil-copy-from-above))

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

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "s-v") 'term-paste)))

(define-key org-mode-map "\C-ck" #'endless/insert-key)
(defun endless/insert-key (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((is-org-mode (derived-mode-p 'org-mode))
         (tag (if is-org-mode
                  "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char (if is-org-mode -8 -6)))))

(provide 'bryan-keybindings)
