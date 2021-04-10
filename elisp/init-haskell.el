;; -*- lexical-binding: t; -*-

(straight-use-package 'dante)
(straight-use-package 'haskell-mode)

(+pdump-packages 'haskell-mode
                 'dante)

;;; haskell-mode

(with-eval-after-load "haskell-mode"
  (require 'smartparens-haskell)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'smartparens-mode))

;;; dante

(autoload #'dante-mode "dante" nil t)

(provide 'init-haskell)
