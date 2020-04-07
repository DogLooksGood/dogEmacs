(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :custom
  (deft-directory "~/deft")
  (deft-extensions '("txt" "org" "md"))
  (deft-auto-save-interval 5.0))

(provide 'init-deft)
