;;; -*- lexical-binding: t; -*-

;; (straight-use-package '(telega :type git :host github :branch "releases"))
(straight-use-package 'telega)

(setq
 telega-chat-fill-column 50
 telega-use-images t
 telega-open-file-function 'org-open-file
 telega-proxies
 `((:server ,+proxy-host :port ,+proxy-port
            :enable t :type (:@type "proxyTypeSocks5"))))

(autoload #'telega "telega")

(defun +telega-open-file (file)
  (cond
   ((member (downcase (file-name-extension file)) '("png" "jpg" "gif" "jpeg"))
    (start-process "telega-open-photo" nil "/sbin/imv" file))
   ((member (downcase (file-name-extension file)) '("mp4"))
    (start-process "telega-open-video" nil "/sbin/mpv" file))
   (t
    (find-file file))))

(with-eval-after-load "telega"
  (setq telega-open-message-as-file '(photo video)
        telega-open-file-function '+telega-open-file)
  (custom-set-faces
   '(telega-entity-type-pre ((t :inherit 'fixed-pitch :family nil))))
  (add-hook 'telega-root-mode-hook '+use-fixed-pitch)
  (add-hook 'telega-root-mode-hook '+load-font))

(provide 'init-telega)
