(use-package ag)

(use-package projectile
  :bind
  ("C-&" . 'projectile-run-async-shell-command-in-root)
  ("C-!" . 'projectile-run-shell-command-in-root)
  ("C-x C-p" . 'counsel-projectile-switch-project)
  ("C-x C-o" . 'counsel-projectile-find-file)
  ("C-x C-x" . 'counsel-projectile-switch-to-buffer)
  ("C-x C-q" . 'projectile-kill-buffers)
  ("C-x /" . 'rg)
  ("C-x C-/" . 'counsel-projectile-rg)
  :init
  (projectile-mode 1))

(use-package counsel-projectile)

(provide 'the-project)
