(defun user/uppercase-sql-keyword ()
  (ignore-errors
    (when (member (char-before)
                  '(41 40 44 59 32 10))
      (save-mark-and-excursion
        (backward-word)
        (when (and (not (nth 3 (syntax-ppss)))
                   (not (nth 4 (syntax-ppss)))
                   (not (let ((face-or-faces (get-text-property (point) 'face)))
                          (if (listp face-or-faces)
                              (or (member 'font-lock-constant-face face-or-faces)
                                  (member 'font-lock-variable-name-face face-or-faces))
                            (or (eq 'font-lock-constant-face face-or-faces)
                                (eq 'font-lock-variable-name-face face-or-faces))))))
          (let ((bounds (bounds-of-thing-at-point 'sexp)))
            (upcase-region (car bounds) (cdr bounds))))))))

(defun user/enable-sql-auto-uppercase-keyword ()
  (interactive)
  (add-hook 'post-self-insert-hook #'user/uppercase-sql-keyword t t)
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w"))

(add-hook 'sql-mode-hook #'user/enable-sql-auto-uppercase-keyword)

(add-hook 'sql-mode-hook 'smartparens-mode)

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
