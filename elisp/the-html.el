;;; -*- lexical-binding: t -*-

(use-package emmet-mode
  :hook (mhtml-mode nxml-mode web-mode)
  :bind
  (:map
   emmet-mode-keymap
   ("M-RET" . 'emmet-expand-line)))

(add-hook 'mhtml-mode-hook 'smartparens-mode)

(provide 'the-html)
