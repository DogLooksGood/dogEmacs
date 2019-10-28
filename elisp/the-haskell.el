(use-package haskell-mode)

(use-package intero
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'the-haskell)
