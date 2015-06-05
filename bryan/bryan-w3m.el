;; w3m.el

;; Otherwise known as "Bryan trying to be a hipster by using a text-based web browser"
;; http://www.emacswiki.org/emacs/emacs-w3m
;; http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
(use-package w3m
  :ensure t
  :config
  (progn
    (setq browse-url-browser-function 'w3m-goto-url-new-session)
    (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
    (setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
    (defun reddit (reddit)
      "Opens the REDDIT in w3m-new-session"
       (interactive (list
                     (read-string "Enter the reddit (default: haskell): " nil nil "haskell" nil)))
       (browse-url (format "http://m.reddit.com/r/%s" reddit)))))

(provide 'bryan-w3m)
