;; -*- lexical-binding: t; -*-

(straight-use-package 'gcmh)

(require 'gcmh)

(gcmh-mode 1)

(setq gcmh-idle-delay 10
      gcmh-high-cons-threshold #x6400000)

(provide 'init-gc)
