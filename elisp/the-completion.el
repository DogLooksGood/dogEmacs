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
  (setq company-idle-delay 1
        company-dabbrev-downcase nil
        company-abort-manual-when-too-short t
        company-require-match nil)
  (global-company-mode 1))

(provide 'the-completion)
