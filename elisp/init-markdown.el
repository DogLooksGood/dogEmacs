;; -*- lexical-binding: t; -*-

(straight-use-package 'markdown-mode)

(+pdump-packages 'markdown-mode)

(setq
 markdown-fontify-code-blocks-natively t)

(with-eval-after-load "markdown-mode"
  (custom-set-faces
   '(markdown-table-face ((t :inherit 'fixed-pitch)))
   '(markdown-code-face ((t :inherit 'fixed-pitch))))

  (add-hook 'markdown-mode-hook #'markdown-toggle-markup-hiding)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode))

(provide 'init-markdown)
