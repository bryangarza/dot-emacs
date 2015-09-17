;; org.el

(require 'org)

(setq org-src-fontify-natively t)

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE")))

(setq org-faces-easy-properties
      '((todo . :background) (tag . :foreground) (priority . :foreground)))

(setq org-todo-keyword-faces
      '(("STARTED" . "yellow")))

(autoload 'org-present "org-present" nil t)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-write)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(use-package toc-org
  :ensure t
  :config
  (progn
    (if (require 'toc-org nil t)
        (add-hook 'org-mode-hook 'toc-org-enable)
      (warn "toc-org not found"))
    (add-to-list 'org-tag-alist '("TOC" . ?T))))

(provide 'bryan-org)
