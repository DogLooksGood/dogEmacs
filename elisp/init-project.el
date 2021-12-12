;;; -*- lexical-binding: t -*-

(setq project-switch-commands '((project-find-file "Find file")
                                (project-find-regexp "Find regexp")
                                (project-dired "Dired")
                                (project-eshell "Eshell")
                                (shell "Shell")
                                (magit-status "Magit")))

(require 'project)

(with-eval-after-load "project"
  (define-key project-prefix-map "m" 'magit-status)
  (define-key project-prefix-map "s" 'shell))

(defun +project-previous-buffer (arg)
  "Toggle to the previous buffer that belongs to current project."
  (interactive "P")
  (unless arg
    (if-let ((pr (project-current)))
      (switch-to-buffer
       (->> (project--buffer-list pr)
         (--remove (or (minibufferp it)
                       (get-buffer-window-list it)))
         (car))))))

(provide 'init-project)
