;;; -*- lexical-binding: t -*-

;;; packages for navigation.

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy))

(use-package phi-search
  :bind
  (
   ("C-s" . 'phi-search)
   :map
   phi-search-default-map
   ("<escape>" . 'phi-search-abort)))

(provide 'init-nav)
