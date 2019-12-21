(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind
  (("M-a" . ace-window))
  :init
  (setq aw-ignore-on nil)
  (setq aw-scope 'frame)
  (setq aw-ignored-buffers '())
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)))

(bind-key "M-e" 'aw-flip-window)
(bind-key "M-h" 'split-window-right)
(bind-key "M-v" 'split-window-below)
(bind-key "M-o" 'delete-other-windows)
(bind-key "M-q" 'delete-window)

(provide 'the-window)
