(use-package ag)

(use-package projectile
  :bind
  ("C-x C-p" . 'counsel-projectile-switch-project)
  ("C-x C-o" . 'counsel-projectile-find-file)
  ("C-x C-x" . 'counsel-projectile-switch-to-buffer)
  ("C-x C-q" . 'projectile-kill-buffers)
  ("C-x r" . 'rg)
  ("C-x C-r" . 'counsel-projectile-rg)
  :init
  (projectile-mode 1))

(use-package counsel-projectile)

(provide 'the-project)
