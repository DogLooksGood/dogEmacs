;; (require 'mhtml-mode)

(use-package emmet-mode
  :hook (mhtml-mode)
  :bind
  (:map mhtml-mode-map
        ("M-RET" . 'emmet-expand-line))
  :init
  (add-hook 'mhtml-mode-hook 'emmet-mode))

(add-hook 'mhtml-mode-hook 'smartparens-mode)

(provide 'the-html)
