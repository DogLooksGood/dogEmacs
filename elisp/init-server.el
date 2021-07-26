(require 'server)

(unless window-system
  (unless (server-running-p)
    (server-start)))

(provide 'init-server)
