(use-package vterm)

(use-package vterm-toggle
  :bind
  (("<f1>" . 'vterm-toggle)
   :map vterm-mode-map ("<f1>" . 'vterm-toggle)))
