;;; -*- lexical-binding: t -*-
;;; Language Server Protocol Support.

(use-package eglot
  :hook
  ((rust-mode c-mode web-mode elixir-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '())
  (eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "/home/tianshu/source/elixir-ls/language_server.sh")))

(provide 'init-lsp)
