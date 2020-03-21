(defun user/lazy-telega ()
  (interactive)
  (use-package telega
    :hook (telega-chat-mode . company-mode)
    :custom
    (telega-proxies '((:server "localhost" :port 1080
                               :enable t :type (:@type "proxyTypeSocks5"))))
    :init
    (telega 1)))

(bind-key "C-S-t" 'user/lazy-telega)

(provide 'the-telegram)
