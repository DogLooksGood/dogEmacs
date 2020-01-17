(provide 'the-javascript)

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook 'smartparens-mode))

(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0))
