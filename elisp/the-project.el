(use-package ag)

(use-package projectile
  :bind
  ("C-x f" . 'counsel-projectile-find-file)
  ("C-x b" . 'counsel-projectile-switch-to-buffer)
  ("C-x k" . 'projectile-kill-buffers)
  ("C-x p" . 'counsel-projectile-switch-project)
  ("C-x r" . 'rg)
  ("C-x s" . 'counsel-projectile-rg)
  :init
  (projectile-mode 1))

(use-package counsel-projectile)

(provide 'the-project)
