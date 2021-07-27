;;; -*- lexical-binding: t -*-

(straight-use-package 'elfeed)

(autoload #'elfeed "elfeed" nil t)

(setq elfeed-curl-extra-arguments `("--socks5-hostname" ,(concat +proxy-host ":" (number-to-string +proxy-port))))

(setq elfeed-feeds '("https://reddit.com/r/clojure.rss"
                     "https://reddit.com/r/rust.rss"
                     "https://www.solidot.org/index.rss"))

(provide 'init-rss)
