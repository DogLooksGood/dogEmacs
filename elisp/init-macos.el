;;; -*- lexical-binding: t -*-

(when (eq 'ns window-system)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ring-bell-function 'ignore)
  (setq-default ns-use-proxy-icon nil)
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(provide 'init-macos)
