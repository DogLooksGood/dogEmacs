(bind-key "C-\\" #'eshell)

(defun eshell/ff (&rest args)
  (apply #'find-file args))

(defun eshell/h (&rest args)
  (apply #'eshell/ls "-lh" args))

(provide 'the-eshell)
