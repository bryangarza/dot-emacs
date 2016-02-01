(defun bryan/hydra ()
  (defun bryan/define-hydras ()

    (defun hydra-move-splitter-left (arg)
      "Move window splitter left."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (shrink-window-horizontally arg)
        (enlarge-window-horizontally arg)))

    (defun hydra-move-splitter-right (arg)
      "Move window splitter right."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'right))
          (enlarge-window-horizontally arg)
        (shrink-window-horizontally arg)))

    (defun hydra-move-splitter-up (arg)
      "Move window splitter up."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (enlarge-window arg)
        (shrink-window arg)))

    (defun hydra-move-splitter-down (arg)
      "Move window splitter down."
      (interactive "p")
      (if (let ((windmove-wrap-around))
            (windmove-find-other-window 'up))
          (shrink-window arg)
        (enlarge-window arg)))

    (defhydra bryan/window-stuff-hydra (:hint nil)
      "
          Split: _v_ert  _s_:horz
     Reorganize: _t_oggle split  _w_indow rotate
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right"

      ;; ("v" split-window-right)
      ;; ("s" split-window-below)
      ("v" split-vert-and-switch)
      ("s" split-horiz-and-switch)

      ("t" toggle-window-split)
      ("w" rotate-windows)

      ("c" delete-window)
      ("o" delete-other-windows :exit t)

      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)

      ("p" previous-buffer)
      ("n" next-buffer)
      ("b" ido-switch-buffer)
      ("f" counsel-find-file)

      ("u" winner-undo)
      ("r" winner-redo)

      ("H" hydra-move-splitter-left)
      ("J" hydra-move-splitter-down)
      ("K" hydra-move-splitter-up)
      ("L" hydra-move-splitter-right)

      ("q" nil))

    (defhydra bryan/multiple-cursors-hydra (:hint nil)
      "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))

(defhydra bryan/org-hydra (:color red :hint nil)
  "
Navigation^
---------------------------------------------------------
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t)
  ("q" nil))

(defhydra hydra-org (:color red :hint nil)
  "
Navigation^
---------------------------------------------------------
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t)
  ("q" nil))

(defhydra bryan/avy-hydra (:color blue)
  "avy-goto"
  ("c" avy-goto-char "char")
  ("C" avy-goto-char-2 "char-2")
  ("l" avy-goto-line "line")
  ("w" avy-goto-word-1 "word")
  ("W" avy-goto-word-0 "word-0")
  ("q" nil))

(defhydra bryan/hydra-hydra (:color blue)
  "
Hydra for hydras
----------------
[_w_] Window stuff
[_c_] Multiple cursors
[_o_] Org mode
[_a_] Avy
"
("w" bryan/window-stuff-hydra/body)
("c" bryan/multiple-cursors-hydra/body)
("o" bryan/org-hydra/body)
("a" bryan/avy-hydra/body)
("q" nil)))

(use-package hydra
  :ensure t
  :config
  (progn
    (bryan/define-hydras))))

(provide 'bryan-hydra)
