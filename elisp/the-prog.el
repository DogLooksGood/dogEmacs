(defun user/singlequote ()
  "If behind a singlequote, convert it to a pair of doublequote.
Otherwise will insert a singlequote."
  (interactive)
  (if (equal (char-before) 39)
      (progn
        (save-mark-and-excursion
          (backward-delete-char 1)
          (insert "\""))
        (insert "\""))
    (insert "'")))

(bind-key "'" #'user/singlequote)

(use-package smartparens
  :config
  (sp-with-modes
      '(rust-mode go-mode java-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package lsp-mode
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp :commands company-lsp)

(provide 'the-prog)
