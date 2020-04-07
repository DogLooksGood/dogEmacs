;;; -*- lexical-binding: t -*-

(use-package yaml-mode)

(use-package toml-mode)

(require 'conf-mode)

(add-hook 'conf-mode 'smartparens-mode)

(provide 'init-conf)
