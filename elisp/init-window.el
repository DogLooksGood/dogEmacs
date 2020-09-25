(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind
  (("M-<tab>" . ace-window))
  :init
  (setq aw-ignore-on nil)
  (setq aw-char-position 'top-left)
  (setq aw-scope 'frame)
  (setq aw-ignored-buffers '())
  (setq aw-keys '(?a ?o ?e ?u ?i)))

;; (require 'winner)
;; (winner-mode 1)

(provide 'init-window)
