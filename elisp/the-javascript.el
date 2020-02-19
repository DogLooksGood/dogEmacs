(provide 'the-javascript)

;; (use-package js2-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-hook 'js2-mode-hook 'smartparens-mode))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-hook 'web-mode-hook 'smartparens-mode)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

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
