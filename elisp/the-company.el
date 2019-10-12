;;; Here we use tab to trigger the auto completion.
;;; Also we use tab to escape the string.


(defun user/tab ()
  (interactive)
  (if (nth 3 (syntax-ppss))
      (paredit-forward-up)
    (company-indent-or-complete-common)))

(use-package company
  :ensure t
  :bind
  (("<tab>" . 'user/tab)
   :map company-active-map
	("}" . 'company-select-next)
	("{" . 'company-select-previous)
	("<escape>" . 'company-abort))
  :init
  (setq company-idle-delay nil)
  (global-company-mode 1))

(provide 'the-company)
