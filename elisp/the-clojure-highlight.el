(defconst user/clojure-font-lock-keywords
  (eval-when-compile
    `(;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                (concat "\\(" clojure--sym-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'clojure-character-face)

      (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--sym-regexp "\\)")
       (1 'clojure-keyword-face)
       (2 'clojure-keyword-face))

      ;; #_ and (comment ...) macros.
      (clojure--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight [[var]] comments
      (,(rx "[[" (group-n 1 (optional "#'")
                          (+ (or (syntax symbol) (syntax word)))) "]]")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      (clojure-font-lock-escaped-chars 0 'bold prepend)
      ;; Highlight grouping constructs in regular expressions
      (clojure-font-lock-regexp-groups
       (1 'font-lock-regexp-grouping-construct prepend)))))

;;;###autoload
(setq clojure-font-lock-keywords user/clojure-font-lock-keywords)

(provide 'the-clojure-highlight)
