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

(progn
  (bind-key ";" 'ace-window special-mode-map)
  (bind-key "\\" 'split-window-right special-mode-map)
  (bind-key "'" 'delete-other-windows special-mode-map)
  (bind-key "q" 'user/delete-window-or-change-buffer special-mode-map)
  (bind-key "-" 'split-window-below special-mode-map))

(provide 'the-window)
