(use-package smartparens
  :config
  (sp-with-modes
      '(rust-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(provide 'the-parens)
