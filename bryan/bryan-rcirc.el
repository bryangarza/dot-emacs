;; rcirc.el

(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#haskell" "#emacs"))))

(setq rcirc-authinfo '(("freenode" nickserv ,freenode-username ,freenode-password)))

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

(provide 'bryan-rcirc)
