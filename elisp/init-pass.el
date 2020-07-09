;;; -*- lexical-binding: t -*-

(defun +pass ()
  (interactive)
  (unless (featurep 'pass)
    (use-package pass
      :init
      (use-package ivy-pass)))
  (call-interactively #'pass))

(bind-key "C-S-p" '+pass global-map)

(provide 'init-pass)
