;;; Here we use tab to trigger the auto completion.
;;; Also we use tab to escape the string.

(use-package yasnippet)

(use-package company
  :ensure t
  :bind
  (:map company-active-map
	("}" . 'company-select-next)
	("{" . 'company-select-previous)
	("<escape>" . 'company-abort))
  :init
  (setq company-idle-delay nil)
  (global-company-mode 1))

(provide 'the-completion)
