;;; -*- lexical-binding: t -*-

(straight-use-package 'elfeed)

(autoload #'elfeed "elfeed" nil t)

(setq elfeed-feeds '("https://www.solidot.org/"
                     "http://feeds.feedburner.com/google/RzFQ?format=xml")
      elfeed-curl-extra-arguments nil)

(provide 'init-rss)
