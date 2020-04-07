;;; Language Server Protocol Support.
;;; Use it for static typed languages.

(use-package eglot
  :hook
  ((rust-mode c-mode) . eglot-ensure)
  :custom
  (eglot-stay-out-of '(flymake))
  (eglot-ignored-server-capabilites '(:documentHighlightProvider)))

(provide 'init-lsp)
