;;; Vterm

(defun +project-open-vterm ()
  (interactive)
  (vterm--set-directory (vc-root-dir))
  (vterm))

(use-package vterm
  :bind
  (("C-c C-p RET" . +project-open-vterm)
   :map
   vterm-mode-map
   ("C-\\" . 'rime-toggle)))

(provide 'init-vterm)
