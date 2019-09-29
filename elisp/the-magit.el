(use-package magit
  :bind
  ("C-z" . 'magit-status))

(use-package diff-hl
  :init
  (global-diff-hl-mode t))

(provide 'the-magit)
