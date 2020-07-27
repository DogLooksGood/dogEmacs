;;; -*- lexical-binding: t -*-

(setq-default js-indent-level 2)

(use-package web-mode
  :hook (web-mode . smartparens-mode)
  :mode ("\\.js\\'" . web-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2))

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
