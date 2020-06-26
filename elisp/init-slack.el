;;; -*- lexical-binding: t -*-

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
 :config
  (slack-register-team
   :name "clojurians"
   :default t
   :token (auth-source-pass-get 'secret "clojurians.slack.com/doglooksgood@gmail.com")
   :full-and-display-names t))

(provide 'init-slack)
