;;; -*- lexical-binding: t; -*-

(straight-use-package '(telega :type git :host github :branch "releases"))

(setq
 telega-use-images t
 telega-open-file-function 'org-open-file
 telega-proxies '((:server "localhost" :port 1089 :enable t :type (:@type "proxyTypeSocks5"))))

(autoload #'telega "telega")

(with-eval-after-load "telega"
  (setcdr (assq t org-file-apps-gnu) 'browse-url-xdg-open)
  (custom-set-faces
   '(telega-entity-type-pre ((t :inherit 'fixed-pitch :family nil))))
  (add-hook 'telega-chat-mode-hook 'visual-line-mode)
  (add-hook 'telega-root-mode-hook '+load-font))

(provide 'init-telega)
