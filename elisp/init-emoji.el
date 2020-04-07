(use-package emojify
  :bind
  ("C-S-e" . 'emojify-insert-emoji)
  :init
  (global-emojify-mode))

(provide 'init-emoji)
