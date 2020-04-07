(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind
  (("C-;" . ace-window)
   ("M-<tab>" . ace-window))
  :init
  (setq aw-ignore-on nil)
  (setq aw-scope 'frame)
  (setq aw-ignored-buffers '())
  (setq aw-keys '(?d ?h ?t ?n ?s)))

(provide 'init-window)
