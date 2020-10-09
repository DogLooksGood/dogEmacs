(use-package ripgrep
  :bind
  (:map
   ripgrep-search-mode-map
   ("w" . 'wgrep-change-to-wgrep-mode)))

(use-package projectile
  :bind
  (:map projectile-mode-map
   ("C-c C-p" . 'projectile-command-map))
  :init
  (projectile-global-mode 1))

(provide 'init-project)
