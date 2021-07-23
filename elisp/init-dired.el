;;; -*- lexical-binding: t; -*-

(straight-use-package 'dired-git-info)
(straight-use-package 'dired-sidebar)

(+pdump-packages 'dired-git-info)

;; dired

(setq dired-dwim-target t)
(setq dired-listing-switches "-lXGh --group-directories-first")

(with-eval-after-load "dired"
  (define-key dired-mode-map "w" #'wdired-change-to-wdired-mode))

;;; dired-git-info

(setq
 dgi-auto-hide-details-p nil)

(autoload #'dired-git-info "dired-git-info")

(with-eval-after-load "dired-git-info"
  (define-key dired-mode-map (kbd "v") 'dired-git-info-mode))


(provide 'init-dired)
