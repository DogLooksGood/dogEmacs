;; -*- lexical-binding: t; -*-

(straight-use-package 'markdown-mode)

(setq
 markdown-fontify-code-blocks-natively t)

(defun +md-init ()
  (variable-pitch-mode 1))

(add-hook 'markdown-mode-hook #'+md-init)

(setq markdown-list-item-bullets '("•" "·"))

(with-eval-after-load "markdown-mode"
  (add-hook 'org-mode-hook 'flyspell-mode)

  (custom-set-faces
   '(markdown-table-face ((t :inherit 'fixed-pitch)))
   '(markdown-code-face ((t :inherit 'fixed-pitch))))

  (add-hook 'markdown-mode-hook #'markdown-toggle-markup-hiding)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode))

(provide 'init-markdown)
