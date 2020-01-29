(use-package ripgrep
  :bind
  (:map
   ripgrep-search-mode-map
   ("w" . 'wgrep-change-to-wgrep-mode)))

(use-package projectile
  :init
  (projectile-global-mode 1))

(use-package counsel-projectile)

;; (require 'project)
;;
;; (setq project-vc-ignores '(".gitignore"))

(provide 'the-project)
