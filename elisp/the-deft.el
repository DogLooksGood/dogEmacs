(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :init
  (setq deft-directory "~/deft"
        deft-extensions '("org" "md")))

(provide 'the-deft)
