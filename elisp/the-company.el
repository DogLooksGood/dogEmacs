(use-package company
  :ensure t
  :bind
  (("<tab>" . 'company-indent-or-complete-common)
   :map company-active-map
	("}" . 'company-select-next)
	("{" . 'company-select-previous)
	("<escape>" . 'company-abort))
  :init
  (setq company-idle-delay nil)
  (global-company-mode 1))

;; (use-package company-posframe
;;   :init
;;   (company-posframe-mode 1))

(provide 'the-company)
