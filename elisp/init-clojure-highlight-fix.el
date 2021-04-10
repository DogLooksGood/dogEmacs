;;; This file overwrites how clojure-mode do highlighting
;;; This patch remove some unnecessary highlight rules.

(defconst +clojure-font-lock-keywords
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
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "as->" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns"
            "with-open" "with-local-vars" "binding"
            "with-redefs" "with-redefs-fn"
            "declare") t)
         "\\>")
       1 font-lock-keyword-face)
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
(setq clojure-font-lock-keywords +clojure-font-lock-keywords)

(provide 'init-clojure-highlight-fix)
