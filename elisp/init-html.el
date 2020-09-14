;;; -*- lexical-binding: t -*-

(require 'mhtml-mode)

(use-package web-mode
  :hook (web-mode . smartparens-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :custom
  (web-mode-engines-alist '(("elixir" . "\\.ex\\'"))))

(use-package emmet-mode
  :hook (mhtml-mode nxml-mode web-mode)
  :bind
  (:map emmet-mode-keymap
   ("M-RET" . 'emmet-expand-line)))

(add-hook 'mhtml-mode-hook 'smartparens-mode)

(provide 'init-html)
