;;; -*- lexical-binding: t -*-

(when (eq 'ns window-system)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(provide 'init-macos)
