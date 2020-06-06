;;; -*- lexical-binding: t -*-
;;; Vterm

(use-package vterm
  :bind
  (("C-c C-p RET" . projectile-run-vterm)
   :map
   vterm-mode-map
   ("C-\\" . 'rime-toggle)))

(provide 'init-vterm)
