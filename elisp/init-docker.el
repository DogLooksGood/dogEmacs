;;; -*- lexical-binding: t -*-

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker
  :bind
  (("C-S-d" . docker)))

(provide 'init-docker)
