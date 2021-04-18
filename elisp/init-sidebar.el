;;; -*- lexical-binding: t -*-

(straight-use-package 'treemacs)

(+pdump-packages 'treemacs)

(defun +treemacs-scale-font-size ()
  (face-remap-add-relative 'default :height 0.8)
  (face-remap-add-relative 'hl-line :box '(:line-width (-1 . -1))))

(setq
 treemacs-no-png-images t
 treemacs-width 40)

(autoload #'treemacs "treemacs" nil t)
(autoload #'treemacs-select-window "treemacs" nil t)

(with-eval-after-load "treemacs"
  (add-hook 'treemacs-mode-hook #'+treemacs-scale-font-size)
  (add-hook 'treemacs-mode-hook #'+set-no-other-window))

(provide 'init-sidebar)
