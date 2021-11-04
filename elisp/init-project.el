(setq project-switch-commands '((project-find-file "Find file")
                           (project-find-regexp "Find regexp")
                           (project-dired "Dired")
                           (project-eshell "Eshell")
                           (shell "Shell")
                           (magit-status "Magit")))

(require 'project)

(with-eval-after-load "project"
  (define-key project-prefix-map "m" 'magit-status)
  (define-key project-prefix-map "s" 'shell))

(provide 'init-project)
