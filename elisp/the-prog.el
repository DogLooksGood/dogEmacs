(bind-key "'" #'user/singlequote)

(use-package smartparens
  :config
  (sp-with-modes
      '(rust-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp)

(use-package company-lsp :commands company-lsp)

(provide 'the-prog)
