(require 'server)

;; Ignore the error when server start.
(ignore-errors
  (unless (server-running-p)
    (server-start)))

(bind-key "C-#" 'server-edit)

(provide 'the-server)
