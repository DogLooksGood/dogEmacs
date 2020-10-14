;;; -*- lexical-binding: t -*-
;;; Language Server Protocol Support.

(defun +lsp-xfind-or-dumb-jump ()
  (interactive)
  (condition-case err
      (call-interactively #'xref-find-definitions)
    (t (call-interactively #'dumb-jump-go))))

(use-package eglot
  :bind
  (:map eglot-mode-map ("M-." . '+lsp-xfind-or-dumb-jump))
  :hook
  ((rust-mode c-mode elixir-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '())
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "/home/tianshu/source/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer")))

(provide 'init-lsp)
