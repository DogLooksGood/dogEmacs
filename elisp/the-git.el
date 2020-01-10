(use-package magit
  :commands (magit-status)
  :bind
  ("C-M-G" . 'magit-status))

(use-package diff-hl
  :bind
  ("C-S-G" . 'diff-hl-mode))

(use-package gitignore-mode
  :init
  (add-hook 'gitignore-mode-hook 'yas-minor-mode))

(provide 'the-git)
