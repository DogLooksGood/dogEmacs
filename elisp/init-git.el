(use-package magit
  :commands (magit-status)
  :bind
  ("C-M-G" . 'magit-status))

(use-package diff-hl
  :bind
  ("C-S-G" . 'diff-hl-mode))

(use-package gitignore-mode
  :hook (gitignore-mode . yas-minor-mode))

(bind-key "C-@" 'smerge-vc-next-conflict global-map)

(provide 'init-git)
