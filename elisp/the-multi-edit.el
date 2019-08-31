(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/skip-to-next-like-this))
  :config
  (add-to-list 'mc/cmds-to-run-once 'god-local-mode)
  (add-to-list 'mc/cmds-to-run-once 'repeat)
  :init
  (setq mc/always-run-for-all nil)
  (multiple-cursors-mode 1))

(use-package highlight-symbol
  :ensure t
  :custom-face
  (highlight-symbol-face ((t (:underline "#666"))))
  :bind
  (("M-n" . 'highlight-symbol-next)
   ("M-p" . 'highlight-symbol-prev))
  :init
  (highlight-symbol-mode 1))

(provide 'the-multi-edit)
