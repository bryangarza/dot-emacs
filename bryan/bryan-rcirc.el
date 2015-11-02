;; rcirc.el

(setq rcirc-default-nick      "wolfcore"
      rcirc-default-user-name "wolfcore"
      rcirc-default-full-name "wolfcore"
      rcirc-prompt            "%n> "
      rcirc-omit-responses    '("JOIN" "PART" "QUIT" "NICK" "AWAY")
      rcirc-auto-authenticate-flag t
      rcirc-server-alist '(("chat.freenode.net"
                            :port 6697
                            :encryption tls
                            :channels ("#haskell"
                                       "#haskell-beginners"
                                       "#emacs"
                                       "#emacs-beginners"))))

(rcirc-track-minor-mode 1)
(set-face-foreground 'rcirc-prompt "#d7ff00")
(set-face-background 'rcirc-prompt nil)

(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively)
                 8192)))

(defun-rcirc-command reconnect (arg)
  "Reconnect the server process."
  (interactive "i")
  (if (buffer-live-p rcirc-server-buffer)
      (with-current-buffer rcirc-server-buffer
        (let ((reconnect-buffer (current-buffer))
              (server (or rcirc-server rcirc-default-server))
              (port (if (boundp 'rcirc-port) rcirc-port rcirc-default-port))
              (nick (or rcirc-nick rcirc-default-nick))
              channels)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (equal reconnect-buffer rcirc-server-buffer)
                (remove-hook 'change-major-mode-hook
                             'rcirc-change-major-mode-hook)
                (let ((server-plist (cdr (assoc-string server rcirc-server-alist))))
                  (when server-plist
                    (setq channels (plist-get server-plist :channels))))
                )))
          (if process (delete-process process))
          (rcirc-connect server port nick
                         nil
                         nil
                         channels)))))

(defun rcirc-change-title (&rest dontcare)
  (if (string= rcirc-activity-string "[]")
      (setq frame-title-format "No IRC activity.")
    (setq frame-title-format rcirc-activity-string))
  (redisplay))
(add-hook 'rcirc-update-activity-string-hook 'rcirc-change-title)

(provide 'bryan-rcirc)
