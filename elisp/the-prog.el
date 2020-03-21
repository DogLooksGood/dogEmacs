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

(use-package smartparens
  :bind
  (:map smartparens-mode-map
        ("C-k" . 'sp-kill-hybrid-sexp)
        ("C-)" . 'sp-forward-slurp-sexp)
        ("C-}" . 'sp-forward-barf-sexp))
  :config
  (sp-with-modes
      '(rust-mode go-mode java-mode rjsx-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil))

(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '()))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  :custom
  (lsp-ui-flycheck-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-flycheck-live-reporting nil)
  (lsp-ui-doc-enable nil))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy))

(provide 'the-prog)
