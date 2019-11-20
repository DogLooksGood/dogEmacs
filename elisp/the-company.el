(use-package company
  :ensure t
  :bind
  (:map company-active-map
	("}" . 'company-select-next)
	("{" . 'company-select-previous)
	("<escape>" . 'company-abort))
  :init
  (setq company-idle-delay nil)
  (add-hook 'prog-mode-hook 'company-mode))

(provide 'the-completion)
