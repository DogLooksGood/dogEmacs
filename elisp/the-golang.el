(use-package go-mode
  :init
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (setq exec-path (append exec-path (list
                                     (concat (user/emacs-dir) "/go/bin")))))

(provide 'the-golang)
