;;; -*- lexical-binding: t -*-

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :custom
  (deft-directory "~/Notes")
  (deft-extensions '("txt" "org" "md"))
  (deft-auto-save-interval 5.0))

(provide 'init-deft)
;;; init-deft.el ends here
