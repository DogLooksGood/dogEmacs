(defconst user/clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Top-level variable definition
      ;; (,(concat "(\\(?:clojure.core/\\)?\\("
      ;;           (regexp-opt '("def" "defonce"))
      ;;           ;; variable declarations
      ;;           "\\)\\>"
      ;;           ;; Any whitespace
      ;;           "[ \r\n\t]*"
      ;;           ;; Possibly type or metadata
      ;;           "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
      ;;           "\\(\\sw+\\)?")
      ;;  (1 font-lock-keyword-face)
      ;;  (2 font-lock-variable-name-face nil t))
      ;; Type definition
      ;; (,(concat "(\\(?:clojure.core/\\)?\\("
      ;;           (regexp-opt '("defstruct" "deftype" "defprotocol"
      ;;                         "defrecord"))
      ;;           ;; type declarations
      ;;           "\\)\\>"
      ;;           ;; Any whitespace
      ;;           "[ \r\n\t]*"
      ;;           ;; Possibly type or metadata
      ;;           "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
      ;;           "\\(\\sw+\\)?")
      ;;  (1 font-lock-keyword-face)
      ;;  (2 font-lock-type-face nil t))
      ;; Function definition (anything that starts with def and is not
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
      ;; lambda arguments - %, %&, %1, %2, etc
      ;; ("\\<%[&1-9]?" (0 font-lock-variable-name-face))
      ;; Special forms
      ;; (,(concat
      ;;    "("
      ;;    (regexp-opt
      ;;     '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
      ;;       "recur" "throw" "try" "catch" "finally"
      ;;       "set!" "new" "."
      ;;       "monitor-enter" "monitor-exit" "quote") t)
      ;;    "\\>")
      ;;  1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      ;; (,(concat
      ;;    "(\\(?:clojure.core/\\)?"
      ;;    (regexp-opt
      ;;     '("letfn" "case" "cond" "cond->" "cond->>" "condp"
      ;;       "for" "when" "when-not" "when-let" "when-first" "when-some"
      ;;       "if-let" "if-not" "if-some"
      ;;       ".." "->" "->>" "as->" "doto" "and" "or"
      ;;       "dosync" "doseq" "dotimes" "dorun" "doall"
      ;;       "ns" "in-ns"
      ;;       "with-open" "with-local-vars" "binding"
      ;;       "with-redefs" "with-redefs-fn"
      ;;       "declare") t)
      ;;    "\\>")
      ;;  1 font-lock-keyword-face)
      ;; Macros similar to let, when, and while
      ;; (,(rx symbol-start
      ;;       (or "let" "when" "while") "-"
      ;;       (1+ (or (syntax word) (syntax symbol)))
      ;;       symbol-end)
      ;;  0 font-lock-keyword-face)
      ;; (,(concat
      ;;    "\\<"
      ;;    (regexp-opt
      ;;     '("*1" "*2" "*3" "*agent*"
      ;;       "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
      ;;       "*command-line-args*" "*compile-files*"
      ;;       "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
      ;;       "*e" "*err*" "*file*" "*flush-on-newline*"
      ;;       "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
      ;;       "*print-dup*" "*print-length*" "*print-level*"
      ;;       "*print-meta*" "*print-readably*"
      ;;       "*read-eval*" "*source-path*"
      ;;       "*unchecked-math*"
      ;;       "*use-context-classloader*" "*warn-on-reflection*")
      ;;     t)
      ;;    "\\>")
      ;;  0 font-lock-builtin-face)
      ;; Dynamic variables - *something* or @*something*
      ;; (,(concat "\\(?:\\<\\|/\\)@?\\(\\*" clojure--sym-regexp "\\*\\)\\>")
      ;;  1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'clojure-character-face)

      ;; namespace definitions: (ns foo.bar)
      ;; (,(concat "(\\<ns\\>[ \r\n\t]*"
      ;;           ;; Possibly metadata, shorthand and/or longhand
      ;;           "\\(?:\\^?\\(?:{[^}]+}\\|:[^ \r\n\t]+[ \r\n\t]\\)[ \r\n\t]*\\)*"
      ;;           ;; namespace
      ;;           "\\(" clojure--sym-regexp "\\)")
      ;;  (1 font-lock-type-face))

      ;; TODO dedupe the code for matching of keywords, type-hints and unmatched symbols

      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      ;; (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
      ;;  (1 'clojure-keyword-face)
      ;;  (2 font-lock-type-face)
      ;;  ;; (2 'clojure-keyword-face)
      ;;  (3 'default)
      ;;  (4 'clojure-keyword-face))
      (,(concat "\\(:\\{1,2\\}\\)\\("
                clojure--sym-regexp
                "?\\)\\(/\\)\\("
                clojure--sym-regexp
                "\\)")
       (0 'clojure-keyword-face))
      (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--sym-regexp "\\)")
       (1 'clojure-keyword-face)
       (2 'clojure-keyword-face))

      ;; type-hints: #^oneword
      ;; (,(concat "\\(#?\\^\\)\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
      ;;  (1 'default)
      ;;  (2 font-lock-type-face)
      ;;  (3 'default)
      ;;  (4 'default))
      ;; (,(concat "\\(#?\\^\\)\\(" clojure--sym-regexp "\\)")
      ;;  (1 'default)
      ;;  (2 font-lock-type-face))

      ;; clojure symbols not matched by the previous regexps; influences CIDER's
      ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
      ;; (,(concat "\\(" clojure--sym-regexp "?\\)\\(/\\)\\(" clojure--sym-regexp "\\)")
      ;;  (1 font-lock-type-face)
      ;;  ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
      ;;  ;; CDSH seems to kick in only for functions and variables referenced w/o
      ;;  ;; writing their namespaces.
      ;;  (2 nil)
      ;;  (3 nil))
      ;; (,(concat "\\(" clojure--sym-regexp "\\)")
      ;;  ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
      ;;  (1 nil))

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


(provide 'the-clojure-highlight)
