(straight-use-package 'docker)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)

(+pdump-packages 'dockerfile-mode
                 'docker-compose-mode
                 'docker)

(autoload #'docker "docker" nil t)




(provide 'init-docker)
