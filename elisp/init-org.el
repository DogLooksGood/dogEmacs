;;; -*- lexical-binding: t -*-

(use-package org
  :hook (org-mode . org-indent-mode)
  :commands (org-mode)
  :config
  (require 'ox-md)
  (add-to-list 'org-modules 'org-tempo t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  :custom
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-export-html-postamble nil))

(use-package org-superstar
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  :custom
  (org-superstar-special-todo-items t))

(use-package valign
  :quelpa
  (valign :repo "casouri/valign" :fetcher github)
  :init
  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'markdown-mode-hook #'valign-mode))

(provide 'init-org)
