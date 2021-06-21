;; -*- lexical-binding: t -*-

(straight-use-package 'flymake)
(straight-use-package 'eglot)

(+pdump-packages 'flymake
                 'eglot)

;;; flymake

(autoload #'flymake-mode "flymake" nil t)

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;;; eglot

(setq
 eglot-stay-out-of nil
 eglot-ignored-server-capabilites '(:documentHighlightProvider))

(autoload #'eglot-ensure "eglot" nil t)

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs
               '(elixir-mode "/usr/lib/elixir-ls/language_server.sh"))
  (add-to-list 'eglot-server-programs
               '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs
               '(clojure-mode "clojure-lsp")))

(provide 'init-lsp)
