(use-package clojure-mode
  :bind
  (:map clojure-mode-map
        ("C-#" . 'user/clojure-comment-block)
        ("C-c C-i" . 'cider-inspect-last-result))
  :init
  (setq clojure-toplevel-inside-comment-form t)
  :config
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (setq clojure-indent-style 'always-indent)
  (add-to-list 'clojure-font-lock-keywords
               `(,(concat "\\(:\\{1,2\\}\\)\\("
                          clojure--sym-regexp
                          "?\\)\\(/\\)\\("
                          clojure--sym-regexp
                          "\\)")
                 (0 'clojure-keyword-face))))

(defun user/clojure-comment-block ()
  (interactive)
  (save-mark-and-excursion
    (when (string-prefix-p "#_" (buffer-substring-no-properties (point) (line-end-position)))
      (forward-char 2))
    (unless
        (or (equal (char-after) 40)
            (equal (char-after) 91)
            (equal (char-after) 123))
      (backward-up-list))
    (if (string-suffix-p "#_" (buffer-substring-no-properties (line-beginning-position) (point)))
        (backward-delete-char 2)
      (insert "#_"))))

(defun user/cljr-setup ()
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (unbind-key "/" clj-refactor-map)
  (clj-refactor-mode 1))

(use-package clj-refactor
  :bind
  (:map
   clojure-mode-map
   ("/" . 'cljr-slash))
  :init
  (add-hook 'clojure-mode-hook 'user/cljr-setup)
  (setq cljr-warn-on-eval t)
  (setq cljr-suppress-middleware-warnings t))

(use-package cider
  :pin melpa-stable
  :commands (cider-jack-in cider-jack-in-cljs cider-jack-in-clj&cljs)
  :bind
  (:map
   cider-mode-map
   ("C-." . 'cider-find-var)
   ("C-c C-n" . 'cider-ns-map)
   :map
   cider-repl-mode-map
   ("(" . 'paredit-open-round)
   (")" . 'paredit-close-round)
   ("[" . 'paredit-open-square)
   ("]" . 'paredit-close-square)
   ("<backspace>" . 'paredit-backward-delete))
  :config
  (unbind-key "M-." cider-mode-map)
  :init
  (add-hook 'cider--debug-mode-hook 'user/insert-mode)
  (setq cider-font-lock-dynamically nil
        cider-font-lock-reader-conditionals nil
        cider-prompt-for-symbol nil
        cider-enhanced-cljs-completion-p nil))

(defun user/clojure-repl-type ()
  (cond
   ((equal major-mode 'clojure-mode) 'clj)
   ((equal major-mode 'clojurescript-mode) 'cljs)))

(defun sesman-current-session (system &optional cxt-types)
  (let ((sessions (or (sesman--linked-sessions system 'sort cxt-types)
                      (sesman--friendly-sessions system 'sort))))
    (if-let ((repl-type (user/clojure-repl-type)))
        (car (--filter (equal repl-type (cider-repl-type (cadr it))) sessions))
      (car sessions))))

(defun user/fulcro-destructing-to-keys (text)
  "text is like {:keys [] :as props}"
  (save-match-data
    (if (string-match "{:\\(.+\\)/keys \\[\\(.+\\)] :as props}" text)
        (let ((ns (match-string 1 text))
              (ks (match-string 2 text)))
          (string-join
           (mapcar (lambda (k) (concat ":" ns "/" k))
                   (split-string ks " "))
           " "))
      "")))

(provide 'the-clojure)
