;;; packages for EDIT

(defun user/mc-toggle-hl-line ()
  (if multiple-cursors-mode
      (hl-line-mode -1)
    (hl-line-mode 1)))

(use-package multiple-cursors
  :bind
  (("C-v" . 'mc/mark-next-like-this)
   ("M-v" . 'mc/skip-to-next-like-this)
   :map mc/keymap
   ("RET" . 'mc/keyboard-quit))
  :init
  (multiple-cursors-mode 1)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (add-hook 'multiple-cursors-mode-hook 'user/mc-toggle-hl-line))

(use-package wgrep
  :bind
  (:map
   wgrep-mode-map
   ("<escape>" . 'm4d-normal-mode)
   ("C-u" . 'm4d-normal-mode)))

(provide 'the-edit)
