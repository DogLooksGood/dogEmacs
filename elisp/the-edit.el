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

(use-package wgrep
  :bind
  (:map
   wgrep-mode-map
   ("<escape>" . 'm4d-normal-mode)
   ("C-u" . 'm4d-normal-mode)))

(provide 'the-edit)
