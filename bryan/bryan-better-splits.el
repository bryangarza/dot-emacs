;; better-splits.el

(defun split-vert-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-horiz-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(provide 'bryan-better-splits)
