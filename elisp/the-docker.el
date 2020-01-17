;;; -*- lexical-binding: t -*-

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker
  :bind
  (("C-S-d" . docker)))

(provide 'the-docker)
