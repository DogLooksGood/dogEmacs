(use-package magit
  :commands (magit-status)
  :bind
  ("C-S-G" . 'magit-status))

(use-package diff-hl
  :init
  (global-diff-hl-mode t))

(use-package gitignore-mode)

(provide 'the-git)
