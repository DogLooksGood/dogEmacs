;;; -*- lexical-binding: t -*-

;;; packages for navigation.

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy))

(use-package phi-search
  :bind
  (("C-s" . 'phi-search)
   :map
   phi-search-default-map
   ("<escape>" . 'phi-search-abort)))

(use-package avy
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(provide 'init-nav)
