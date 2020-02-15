(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :init
  (setq deft-directory "~/deft"
        deft-extensions '("text" "org" "md")
        deft-auto-save-interval 5.0))

(provide 'the-deft)
