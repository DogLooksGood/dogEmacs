(use-package ripgrep
  :bind
  (:map
   ripgrep-search-mode-map
   ("w" . 'wgrep-change-to-wgrep-mode)))

(use-package projectile)

(use-package counsel-projectile
  :bind
  ("C-x C-o" . 'counsel-projectile-find-file)
  ("C-x C-r" . 'projectile-ripgrep)
  ("C-x C-p" . 'counsel-projectile-switch-project)
  ("C-x C-q" . 'projectile-kill-buffers))

(provide 'the-project)
