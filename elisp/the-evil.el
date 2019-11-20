(use-package evil
  :init
  (add-hook 'prog-mode-hook 'evil-local-mode)
  (add-hook 'conf-mode-hook 'evil-local-mode)
  (add-hook 'text-mode-hook 'evil-local-mode))

(provide 'the-evil)
