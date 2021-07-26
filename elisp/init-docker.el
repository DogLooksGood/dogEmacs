(straight-use-package 'docker)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)

(autoload #'docker "docker" nil t)

(provide 'init-docker)
