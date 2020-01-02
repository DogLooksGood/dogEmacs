(use-package company
  :ensure t
  :bind
  (:map company-mode-map
   ("<tab>" . 'user/insert-tab)
   :map company-active-map
   ("}" . 'company-select-next)
   ("{" . 'company-select-previous)
   ("<escape>" . 'company-abort))
  :init
  (setq company-idle-delay nil
        company-dabbrev-downcase nil
        company-abort-manual-when-too-short t
        company-require-match nil
        company-global-modes '(not dired-mode dired-sidebar-mode))
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode)
  (add-hook 'conf-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook 'company-mode))

(provide 'the-completion)
