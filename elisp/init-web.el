;;; -*- lexical-binding: t -*-

(use-package web-mode
  :mode ("\\.html$")
  :hook (web-mode . smartparens-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-pairing t))

(use-package emmet-mode
  :hook (web-mode)
  :bind
  (:map emmet-mode-keymap ("M-RET" . 'emmet-expand-line)))

(provide 'init-web)
