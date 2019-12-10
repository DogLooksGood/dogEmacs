(use-package avy
  :commands (avy-goto-word-or-subword-1)
  :bind
  ("C-z" . 'avy-goto-word-or-subword-1)
  :init
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-setup-default))

(defun user/set-mc-rules ()
  (let ((rules '(god-local-mode repeat)))
    (mapcar (lambda (r) (add-to-list 'mc/cmds-to-run-once r))
            rules))
  (let ((rules '(user/singlequote
                 user/lisp-semicolon
                 user/rust-minus
                 user/rust-semicolon
                 user/rust-lessthan
                 user/rust-whitespace
                 cljr-slash
                 sp-kill-hybrid-sexp
                 forward-sexp
                 backward-sexp
                 paredit-raise-sexp
                 paredit-forward-slurp-sexp
                 paredit-forward-barf-sexp
                 paredit-forward-up
                 paredit-backward-up
                 paredit-forward-down
                 paredit-backward-down
                 transpose-sexps)))
    (mapcar (lambda (r) (add-to-list 'mc/cmds-to-run-for-all r))
            rules)))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/skip-to-next-like-this))
  :config
  (user/set-mc-rules)
  ;; We can use C-v, M-v to cycle cursors.
  (require 'mc-cycle-cursors)
  :init
  (setq mc/always-run-for-all nil)
  (multiple-cursors-mode 1))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

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
