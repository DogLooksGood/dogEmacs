;;; packages for EDIT

(defun +mc-toggle-hl-line ()
  (if multiple-cursors-mode
      (hl-line-mode -1)
    (hl-line-mode 1)))

(use-package multiple-cursors
  :hook
  (multiple-cursors-mode . +mc-toggle-hl-line)
  :bind
  (:map mc/keymap
   ("RET" . 'mc/keyboard-quit))
  :init
  (multiple-cursors-mode 1)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (add-to-list 'mc/cursor-specific-vars 'meow--keypad-keys)
  (add-to-list 'mc/cursor-specific-vars 'meow--keypad-previous-state)
  (add-to-list 'mc/cursor-specific-vars 'meow--prefix-arg)
  (add-to-list 'mc/cursor-specific-vars 'meow--use-literal)
  (add-to-list 'mc/cursor-specific-vars 'meow--use-meta)
  (add-to-list 'mc/cursor-specific-vars 'meow--selection-history)
  (add-to-list 'mc/cursor-specific-vars 'meow--selection)
  (add-to-list 'mc/cursor-specific-vars 'meow--position-history))

(use-package tiny)

(use-package wgrep)

(provide 'init-edit)
