;;; Prefer URxvt
;; (bind-key "C-\\" #'eshell)

(defun eshell/f (&rest args)
  (apply #'find-file args))

(defun eshell/h (&rest args)
  (apply #'eshell/ls "-lh" args))

(defun eshell/d (&rest args)
  (if args
      (apply #'dired args)
    (dired ".")))

(defun eshell/q (&rest args)
  (eshell/exit))

(add-to-list 'user/god-mode-enable-mode-list
             'eshell-mode)

(provide 'the-eshell)
