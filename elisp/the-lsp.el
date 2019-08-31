(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp)

;; optionally
(use-package company-lsp :commands company-lsp)

(provide 'the-lsp)
