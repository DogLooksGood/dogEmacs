;;; packages for EDIT

(use-package multiple-cursors
  :bind
  (("C-v" . 'mc/mark-next-like-this)
   ("M-v" . 'mc/skip-to-next-like-this)
   :map mc/keymap
   ("RET" . 'mc/keyboard-quit))
  :init
  (setq mc/always-run-for-all t)
  (multiple-cursors-mode 1)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package expand-region
  :bind
  ("C-+" . 'er/expand-region))

(use-package easy-kill
  :bind
  (("C-o" . 'easy-mark)
   :map
   easy-kill-base-map
   ("<backspace>" . 'easy-kill-delete-region)
   ("<escape>" . 'easy-kill-abort)
   ("SPC" . 'easy-kill-mark-region))
  :init
  (setq easy-kill-alist
        '((?w word           " ")
          (?s sexp           "\n")
          (?l list           "\n")
          (?d defun          "\n\n")
          (?m line           "\n"))))

(use-package iedit
  :bind
  ("C-z" . 'iedit-mode)
  :init
  (unbind-key "C-;" global-map))

(use-package wgrep
  :bind
  (:map
   wgrep-mode-map
   ("<escape>" . 'user/normal-mode)))

(provide 'the-edit)
