;;; -*- lexical-binding: t -*-
;;; Language Server Protocol Support.

(use-package eglot
  :hook
  ((rust-mode c-mode web-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake))
  (eglot-ignored-server-capabilites '(:documentHighlightProvider)))

(provide 'init-lsp)
