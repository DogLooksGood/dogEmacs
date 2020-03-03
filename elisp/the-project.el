(use-package ripgrep
  :bind
  (:map
   ripgrep-search-mode-map
   ("w" . 'wgrep-change-to-wgrep-mode)))

(use-package projectile
  :bind
  (:map
   projectile-mode-map
   ("C-c C-p" . 'projectile-command-map))
  :init
  (projectile-global-mode 1))

;; (use-package rg
;;   :bind
;;   (("C-c s" . 'rg-menu)
;;    :map
;;    rg-mode-map
;;    ("w" . 'wgrep-change-to-wgrep-mode)))

(provide 'the-project)
