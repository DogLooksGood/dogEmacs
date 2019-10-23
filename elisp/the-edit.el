(use-package avy
  :commands (avy-goto-word-or-subword-1)
  :bind
  ("C-z" . 'avy-goto-word-or-subword-1)
  :init
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-setup-default))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/skip-to-next-like-this))
  :config
  (add-to-list 'mc/cmds-to-run-once 'god-local-mode)
  (add-to-list 'mc/cmds-to-run-once 'repeat)
  (add-to-list 'mc/cmds-to-run-for-all 'cljr-slash)
  (add-to-list 'mc/cmds-to-run-for-all 'user/singlequote)
  (add-to-list 'mc/cmds-to-run-for-all 'user/lisp-semicolon)
  (add-to-list 'mc/cmds-to-run-for-all 'user/rust-minus)
  (add-to-list 'mc/cmds-to-run-for-all 'user/rust-semicolon)
  :init
  (setq mc/always-run-for-all nil)
  (multiple-cursors-mode 1))

(use-package expand-region
  :commands (er/expand-region)
  :bind
  ("C-+" . 'er/expand-region))

(use-package anzu
  :bind
  ("C-%" . 'anzu-query-replace-regexp)
  ("M-%" . 'anzu-query-replace)
  ("C-&" . 'anzu-query-replace-at-cursor-thing)
  :init
  (global-anzu-mode))

(use-package rg
  :commands (rg counsel-projectile-rg))

(use-package wgrep)

(provide 'the-edit)
