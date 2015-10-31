;; org.el

;; (require 'org)
;(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp")
;(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/contrib/lisp")

(setq org-src-fontify-natively t)

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE")))

(setq org-faces-easy-properties
      '((todo . :background) (tag . :foreground) (priority . :foreground)))

;; (setq org-todo-keyword-faces
;;       '(("STARTED" . (:foreground "yellow"))))
(setq org-todo-keyword-faces
      '(("STARTED" . ((,class (:bold t :weight bold :foreground ,yellow :background ,white
                                     :box (:line-width 1 :style none)))))))

(setq org-hide-leading-stars t)

;;; org-bullets.el --- Show bullets in org-mode as UTF-8 characters
;;; Version: 0.2.4
;;; Author: sabof
;;; URL: https://github.com/sabof/org-bullets
(defgroup org-bullets nil
  "Display bullets as UTF-8 characters"
  :group 'org-appearance)

;; A nice collection of unicode bullets:
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
(defcustom org-bullets-bullet-list
  '(;;; Large
    "◉"
    "○"
    "✸"
    "✿"
    ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
    ;; ► • ★ ▸
    )
  "This variable contains the list of bullets.
It can contain any number of symbols, which will be repeated."
  :group 'org-bullets
  :type '(repeat (string :tag "Bullet character")))

(defcustom org-bullets-face-name nil
  "This variable allows the org-mode bullets face to be
 overridden. If set to a name of a face, that face will be
 used. Otherwise the face of the heading level will be used."
  :group 'org-bullets
  :type 'symbol)

(defvar org-bullets-bullet-map
  '(keymap
    (mouse-1 . org-cycle)
    (mouse-2
     . (lambda (e)
         (interactive "e")
         (mouse-set-point e)
         (org-cycle))))
  "Mouse events for bullets.
Should this be undesirable, one can remove them with

\(setcdr org-bullets-bullet-map nil\)")

(defun org-bullets-level-char (level)
  (string-to-char
   (nth (mod (1- level)
             (length org-bullets-bullet-list))
        org-bullets-bullet-list)))

;;;###autoload
(define-minor-mode org-bullets-mode
    "UTF8 Bullets for org-mode"
  nil nil nil
  (let* (( keyword
           `(("^\\*+ "
              (0 (let* (( level (- (match-end 0) (match-beginning 0) 1))
                        ( is-inline-task
                          (and (boundp 'org-inlinetask-min-level)
                               (>= level org-inlinetask-min-level))))
                   (compose-region (- (match-end 0) 2)
                                   (- (match-end 0) 1)
                                   (org-bullets-level-char level))
                   (when is-inline-task
                     (compose-region (- (match-end 0) 3)
                                     (- (match-end 0) 2)
                                     (org-bullets-level-char level)))
                   (when (facep org-bullets-face-name)
                     (put-text-property (- (match-end 0)
                                           (if is-inline-task 3 2))
                                        (- (match-end 0) 1)
                                        'face
                                        org-bullets-face-name))
                   (put-text-property (match-beginning 0)
                                      (- (match-end 0) 2)
                                      'face (list :foreground
                                                  (face-attribute
                                                   'default :background)))
                   (put-text-property (match-beginning 0)
                                      (match-end 0)
                                      'keymap
                                      org-bullets-bullet-map)
                   nil))))))
    (if org-bullets-mode
        (progn
          (font-lock-add-keywords nil keyword)
          (font-lock-fontify-buffer))
      (save-excursion
        (goto-char (point-min))
        (font-lock-remove-keywords nil keyword)
        (while (re-search-forward "^\\*+ " nil t)
          (decompose-region (match-beginning 0) (match-end 0)))
        (font-lock-fontify-buffer))
      )))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))
(require 'ox-bibtex)
;; (setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
(setq org-latex-pdf-process (list "latexmk -pdf %f"))

(provide 'bryan-org)
