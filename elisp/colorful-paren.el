;; parentheses

(defface colorful-paren-round '((t (:inherit shadow)))
  "Face for round paren."
  :group 'font-lock-extra-types
  :group 'faces)

(defface colorful-paren-square
  '((t (:inherit font-lock-keyword-face)))
  "Face for square paren."
  :group 'font-lock-extra-types
  :group 'faces)

(defface colorful-paren-curly '
  ((t (:inherit font-lock-constant-face)))
  "Face for curly paren."
  :group 'font-lock-extra-types
  :group 'faces)

(defconst colorful-paren-round-regexp "[()]")
(defconst colorful-paren-square-regexp "[][]")
(defconst colorful-paren-curly-regexp "[{}]")

(define-minor-mode colorful-paren-mode
  "Use a dedicated face just for parentheses."
  :lighter nil
  (let ((keywords `((,colorful-paren-round-regexp 0 'colorful-paren-round)
                    (,colorful-paren-square-regexp 0 'colorful-paren-square)
                    (,colorful-paren-curly-regexp 0 'colorful-paren-curly))))
    (if colorful-paren-mode
        (font-lock-add-keywords  nil keywords)
      (font-lock-remove-keywords nil keywords)))
  (when font-lock-mode
    (if (and (fboundp 'font-lock-flush)
             (fboundp 'font-lock-ensure))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure))
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(provide 'colorful-paren)
