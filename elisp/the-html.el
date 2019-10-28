(use-package emmet-mode
  :bind
  (:map html-mode-map
        ("M-<return>" . 'emmet-expand-line))
  :init
  (add-hook 'html-mode-hook 'emmet-mode))
