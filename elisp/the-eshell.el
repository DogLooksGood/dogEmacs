;;; Prefer URxvt

(defun eshell/f (&rest args)
  (apply #'find-file-other-window args))

(defun eshell/e (&rest args)
  (apply #'find-file args))

(defun eshell/h (&rest args)
  (apply #'eshell/ls "-lh" args))

(defun eshell/d (&rest args)
  (if args
      (apply #'dired args)
    (dired ".")))

(defun eshell/q (&rest args)
  (eshell/exit))

(defun user/eshell-ls-lha ()
  (interactive)
  (eshell-kill-input)
  (insert "ls -lha")
  (eshell-send-input))

(defun user/eshell-ls-lh ()
  (interactive)
  (eshell-kill-input)
  (insert "ls -lh")
  (eshell-send-input))

(defun user/eshell-setup ()
  (setq-local company-idle-delay nil))

(provide 'the-eshell)
