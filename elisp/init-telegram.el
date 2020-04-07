(defun +lazy-telega ()
  (interactive)
  (use-package telega
    :ensure t
    :hook
    ((telega-chat-mode . company-mode)
     (telega-chat-mode . yas-minor-mode))
    :custom
    (telega-proxies '((:server "localhost" :port 1080
                               :enable t :type (:@type "proxyTypeSocks5"))))
    (telega-emoji-use-images nil)
    :config
    (setq-local rime-show-candidate 'message)
    :init
    (telega 1)
    :custom-face
    (telega-entity-type-code ((t (:family "unifont" :inherit font-lock-constant-face))))
    (telega-entity-type-pre ((t (:family "unifont" :bold t))))))

(bind-key "C-S-t" '+lazy-telega)

(provide 'init-telegram)
