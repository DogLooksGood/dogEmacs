(use-package ripgrep
  :bind
  (:map
   ripgrep-search-mode-map
   ("w" . 'wgrep-change-to-wgrep-mode)))

;; (bind-key "w" 'wgrep-change-to-wgrep-mode xref--xref-buffer-mode-map)

(use-package projectile
  :bind
  (:map projectile-mode-map
   ("C-c C-p" . 'projectile-command-map))
  :init
  (projectile-global-mode 1))

(provide 'init-project)
