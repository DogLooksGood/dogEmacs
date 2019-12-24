;;; packages for EDIT

(defun user/set-mc-rules ()
  (let ((rules '(god-local-mode
                 repeat
                 keyboard-escape-quit)))
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
                 transpose-sexps
                 user/escape)))
    (mapcar (lambda (r) (add-to-list 'mc/cmds-to-run-for-all r))
            rules)))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this)
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/skip-to-next-like-this))
  :config
  (user/set-mc-rules)
  :init
  (setq mc/always-run-for-all nil)
  (multiple-cursors-mode 1)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package expand-region
  :commands (er/expand-region)
  :bind
  ("C-+" . 'er/expand-region))

(use-package easy-kill
  :bind
  (("C-o" . 'easy-kill)
   :map
   easy-kill-base-map
   ("SPC" . 'easy-kill-region)
   ("<backspace>" . 'easy-kill-delete-region))
  :init
  (setq easy-kill-alist
        '((?w word           " ")
          (?s sexp           "\n")
          (?l list           "\n")
          (?F filename       "\n")
          (?d defun          "\n\n")
          (?u defun-name     " ")
          (?L line           "\n")
          (?B buffer-file-name))))

(use-package anzu
  :bind
  ("C-%" . 'anzu-query-replace-regexp)
  ("M-%" . 'anzu-query-replace)
  :init
  (global-anzu-mode))

(use-package rg
  :commands (rg counsel-projectile-rg))

(use-package wgrep)

(provide 'the-edit)
