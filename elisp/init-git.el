;;; -*- lexical-binding: t -*-

(straight-use-package 'diff-hl)
(straight-use-package 'magit)
(straight-use-package 'smerge-mode)

;;; magit

(autoload #'magit "magit" nil t)
(global-set-key (kbd "C-x g") 'magit)

(with-eval-after-load "magit"
  (define-key transient-base-map (kbd "<escape>") #'transient-quit-one))

(autoload #'magit-status "magit" nil t)
(autoload #'magit-diff "magit" nil t)
(autoload #'magit-blame "magit" nil t)

;;; diff-hl

(autoload #'diff-hl-mode "diff-hl")

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'conf-mode-hook 'diff-hl-mode)

;;; smerge

(autoload #'smerge-mode "smerge-mode" nil t)

(add-hook 'find-file-hook 'smerge-mode)

(provide 'init-git)
