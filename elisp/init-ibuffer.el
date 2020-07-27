;;; -*- lexical-binding: t -*-

(use-package ibuffer-sidebar)

(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))


(provide 'init-ibuffer)
