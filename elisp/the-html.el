(use-package emmet-mode
  :bind
  (:map html-mode-map
        ("M-RET" . 'emmet-expand-line))
  :init
  (add-hook 'html-mode-hook 'emmet-mode))

(add-hook 'mhtml-mode-hook 'smartparens-mode)
