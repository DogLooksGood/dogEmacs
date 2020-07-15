;;; -*- lexical-binding: t -*-

(use-package elixir-mode
  :commands (elixir-mode))

(use-package mix
  :hook
  ((elixir-mode) . 'mix-minor-mode)
  :custom
  (compilation-scroll-output t))

(use-package alchemist
  :hook
  (elixir))

(provide 'init-elixir)
