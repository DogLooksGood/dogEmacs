(use-package multiple-cursors
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/skip-to-next-like-this))
  :config
  (add-to-list 'mc/cmds-to-run-once 'god-local-mode)
  (add-to-list 'mc/cmds-to-run-once 'repeat)
  (add-to-list 'mc/cmds-to-run-for-all 'user/singlequote)
  (add-to-list 'mc/cmds-to-run-for-all 'user/lisp-semicolon)
  (add-to-list 'mc/cmds-to-run-for-all 'user/rust-minus)
  (add-to-list 'mc/cmds-to-run-for-all 'user/rust-semicolon)
  :init
  (setq mc/always-run-for-all nil)
  (multiple-cursors-mode 1))

(use-package highlight-symbol
  :custom-face
  (highlight-symbol-face ((t (:underline "#666"))))
  :bind
  (("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :init
  (highlight-symbol-mode 1))

(provide 'the-multi-edit)
