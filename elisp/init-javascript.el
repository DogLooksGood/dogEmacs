;;; -*- lexical-binding: t -*-

(setq-default js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0))

(use-package css-mode
  :hook (css-mode . smartparens-mode)
  :init
  (add-hook 'css-mode-hook 'smartparens-mode))

(use-package json
  :hook (json-mode . smartparens-mode))

(provide 'init-javascript)
;;; init-javascript.el ends here
