(defun user/uppercase-sql-keyword ()
  (let ((init-pos (point)))
    (when (member (char-before)
                  '(41 40 44 59 32 10))
      (save-mark-and-excursion
        (backward-word)
        (if (and (not (nth 3 (syntax-ppss)))
                 (not (nth 4 (syntax-ppss)))
                 (not (let ((face-or-faces (get-text-property (point) 'face)))
                        (if (listp face-or-faces)
                            (or (member 'font-lock-constant-face face-or-faces)
                                (member 'font-lock-variable-name-face face-or-faces))
                          (or (eq 'font-lock-constant-face face-or-faces)
                              (eq 'font-lock-variable-name-face face-or-faces))))))
            (when (<= (save-mark-and-excursion
                        (forward-word 1)
                        (point))
                      init-pos)
              (upcase-word 1)))))))

(defun user/enable-sql-auto-uppercase-keyword ()
  (interactive)
  (add-hook 'post-self-insert-hook #'user/uppercase-sql-keyword t t)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))

(add-hook 'sql-mode-hook #'user/enable-sql-auto-uppercase-keyword)

;;; Add keywords for sql fontlock
(font-lock-add-keywords
 'sql-mode
 '(;; For Postgres
   ("\"[-a-zA-Z0-9_]+\"" . font-lock-constant-face)
   ;; For MySQL
   ("`[-a-zA-z0-9_]+`" . font-lock-constant-face)
   ;; For placeholder
   (":[-a-zA-Z0-9_]+" . font-lock-variable-name-face)))

(provide 'the-sql)
