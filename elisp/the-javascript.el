(provide 'the-javascript)

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook 'smartparens-mode))

(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0))

(use-package css-mode
  :init
  (add-hook 'css-mode-hook 'smartparens-mode))

(setq-default js-indent-level 2)

(use-package json
  :init
  (add-hook 'json-mode-hook 'smartparens-mode))
