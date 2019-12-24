(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind
  (("<M-tab>" . ace-window))
  :init
  (setq aw-ignore-on nil)
  (setq aw-scope 'frame)
  (setq aw-ignored-buffers '())
  (setq aw-keys '(?d ?h ?t ?n ?s)))

;;; rarely used
;;; (bind-key "<M-tab>" 'aw-flip-window)
(bind-key "M-\\" 'split-window-right)
(bind-key "M--" 'split-window-below)
(bind-key "M-o" 'delete-other-windows)
(bind-key "M-q" 'delete-window)

(provide 'the-window)
