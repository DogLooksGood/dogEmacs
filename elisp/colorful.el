;; parentheses

(defface colorful-round '((t (:inherit shadow)))
  "Face for round paren."
  :group 'font-lock-extra-types
  :group 'faces)

(defface colorful-square
  '((t (:inherit font-lock-keyword-face)))
  "Face for square paren."
  :group 'font-lock-extra-types
  :group 'faces)

(defface colorful-curly
  '((t (:inherit font-lock-constant-face)))
  "Face for curly paren."
  :group 'font-lock-extra-types
  :group 'faces)

(defface colorful-semicolon
  '((t (:inherit font-lock-function-name-face)))
  "Face for semicolon."
  :group 'font-lock-extra-types
  :group 'faces)

(defface colorful-question
  '((t (:inherit font-lock-function-name-face :bold t)))
  "Face for semicolon."
  :group 'font-lock-extra-types
  :group 'faces)

(defconst colorful-round-regexp "[()]")
(defconst colorful-square-regexp "[][]")
(defconst colorful-curly-regexp "[{}]")
(defconst colorful-semicolon-regexp "\\(;\\)$")
(defconst colorful-question-regexp "\\?")

(defvar colorful--rules nil)

(add-to-list 'colorful--rules (cons 'round `(,colorful-round-regexp 0 'colorful-round)))
(add-to-list 'colorful--rules (cons 'square `(,colorful-square-regexp 0 'colorful-square)))
(add-to-list 'colorful--rules (cons 'curly `(,colorful-curly-regexp 0 'colorful-curly)))
(add-to-list 'colorful--rules (cons 'semicolon `(,colorful-semicolon-regexp 1 'colorful-semicolon)))
(add-to-list 'colorful--rules (cons 'question `(,colorful-question-regexp 0 'colorful-question)))

(defcustom colorful-mode-rules
  '((clojure-mode round square curly)
    (clojurec-mode round square curly)
    (clojurescript-mode round square curly)
    (emacs-lisp-mode round square)
    (rust-mode semicolon question))
  "Mode to rule mappings")

(defun colorful--get-rules (mode)
  "Get rules by MODE."
  (mapcar
   (lambda (x) (alist-get x colorful--rules))
   (alist-get mode colorful-mode-rules)))

(define-minor-mode colorful-mode
  "Use a dedicated face just for parentheses."
  :lighter nil
  (let ((keywords (colorful--get-rules major-mode)))
    (if colorful-mode
        (font-lock-add-keywords nil keywords)
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

(provide 'colorful)
