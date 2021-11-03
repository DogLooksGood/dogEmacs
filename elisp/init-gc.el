;; -*- lexical-binding: t; -*-

(straight-use-package 'gcmh)

(require 'gcmh)

(setq gcmh-high-cons-threshold #x4000000)

(gcmh-mode 1)

(provide 'init-gc)
