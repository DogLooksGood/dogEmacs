;;; -*- lexical-binding: t -*-

(straight-use-package 'dired-git-info)
(straight-use-package 'diff-hl)
(straight-use-package 'magit)
(straight-use-package 'smerge-mode)

(+pdump-packages 'dired-git-info
                 'diff-hl
                 'magit)

;;; magit

(autoload #'magit "magit")

;;; diff-hl

(autoload #'diff-hl-mode "diff-hl")
;; (autoload #'diff-hl-dired-mode "diff-hl")

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'conf-mode-hook 'diff-hl-mode)

;;; dired-git-info

(setq
 dgi-auto-hide-details-p nil)

(autoload #'dired-git-info "dired-git-info")

(with-eval-after-load "dired-git-info"
  (define-key dired-mode-map (kbd "v") 'dired-git-info-mode))

;;; smerge

(autoload #'smerge-mode "smerge-mode" nil t)

(defun +try-smerge ()
  (save-excursion
  	(goto-char (point-min))
  	(when (re-search-forward "^<<<<<<< " nil t)
  	  (smerge-mode 1))))

(add-hook 'find-file-hook '+try-smerge t)

(provide 'init-git)
