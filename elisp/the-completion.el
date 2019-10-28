(defun user/tab ()
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively 'indent-region))
   ((string-match-p "^[ \t]*$" (buffer-substring-no-properties (line-beginning-position)
                                                             (point)))
    (call-interactively #'indent-for-tab-command))
   (t
    (company-complete-common-or-cycle))))

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

(provide 'the-completion)
