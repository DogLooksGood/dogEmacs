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
  (org-export-html-postamble nil))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)
