;;; packages for EDIT

(use-package multiple-cursors
  :commands (mc/mark-next-like-this)
  :bind
  (("C-v" . 'mc/mark-next-like-this)
   ("M-v" . 'mc/skip-to-next-like-this)
   :map selected-keymap
   ("C-v" . 'mc/mark-all-in-region-regexp))
  :config
  :init
  (setq mc/always-run-for-all t)
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
          (?m line           "\n")
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
