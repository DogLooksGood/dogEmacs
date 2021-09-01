;; -*- lexical-binding: t; -*-

(straight-use-package 'typescript-mode)

(setq typescript-indent-level 2)

(with-eval-after-load "typescript-mode"
  (add-hook 'typescript-mode-hook '+lsp-start)
  (add-hook 'typescript-mode-hook 'smartparens-mode))

(provide 'init-typescript)
