;; -*- lexical-binding: t; -*-
(straight-use-package 'edit-indirect)
(straight-use-package 'markdown-mode)

(setq markdown-fontify-code-blocks-natively t
      markdown-list-item-bullets '("•" "·"))

(with-eval-after-load "markdown-mode"
  (require 'edit-indirect)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook #'markdown-toggle-markup-hiding)
  (add-hook 'markdown-mode-hook #'variable-pitch-mode)
  (define-key markdown-mode-map (kbd "<f5>") #'markdown-toggle-markup-hiding)

  (custom-set-faces
   '(markdown-table-face ((t :inherit 'fixed-pitch)))
   '(markdown-code-face ((t :inherit 'fixed-pitch)))))

(provide 'init-markdown)
