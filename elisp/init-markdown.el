;;; -*- lexical-binding: t -*-

(require 'org)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t))

(provide 'init-markdown)
