(use-package company
  :ensure t
  :bind
  (:map company-active-map
	("}" . 'company-select-next)
	("{" . 'company-select-previous)
	("<escape>" . 'company-abort))
  :init
  (setq company-idle-delay 1
        company-dabbrev-downcase nil
        company-abort-manual-when-too-short t
        company-require-match nil)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-quickhelp
  :after company
  :init
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

(provide 'the-completion)
