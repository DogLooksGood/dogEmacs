(use-package ag)

(use-package wgrep)

(use-package projectile
  :bind
  ("C-c f" . 'counsel-projectile-find-file)
  ("C-c b" . 'counsel-projectile-switch-to-buffer)
  ("C-c k" . 'projectile-kill-buffers)
  ("C-c p" . 'counsel-projectile-switch-project)
  ("C-c a" . 'projectile-ag)
  :init
  (projectile-mode 1))

(use-package counsel-projectile)

(provide 'the-projectile)
