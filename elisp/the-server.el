(require 'server)

(unless (server-running-p)
  (server-start))

(bind-key "C-#" 'server-edit)

(provide 'the-server)
