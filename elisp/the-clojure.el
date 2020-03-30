;;; -*- lexical-binding: t -*-

;;; Custom helpers

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

;;; Custom commands

(defun user/clojure-hide-comment (&rest args)
  (save-mark-and-excursion
    (while (search-forward "(comment" nil t)
      ;; We ignore the comment block in comment.
      (unless (nth 4 (syntax-ppss))
        (hs-hide-block)))))

(defun user/clojure-hash-comment (arg)
  (interactive "P")
  (cond
   ((not arg)
    (save-mark-and-excursion
      (if (equal "#_" (buffer-substring-no-properties (point) (+ 2 (point))))
          (while (equal "#_" (buffer-substring-no-properties (point) (+ 2 (point))))
            (delete-char 2))
        (progn
          (unless (or (equal (char-after) 40)
                      (equal (char-after) 91)
                      (equal (char-after) 123))
            (backward-up-list))
          (if (string-suffix-p "#_" (buffer-substring-no-properties (line-beginning-position) (point)))
              (while (string-suffix-p "#_" (buffer-substring-no-properties (line-beginning-position) (point)))
                (delete-char -2))
            (insert "#_"))))))

   ((numberp arg)
    (let* ((curr-sym (symbol-at-point))
           (curr-sym-name (symbol-name curr-sym))
           (line (buffer-substring-no-properties (point) (line-end-position)))
           (i 0))
      (save-mark-and-excursion
        (when curr-sym
          (unless (string-prefix-p curr-sym-name line)
            (backward-sexp))
          (while (< i current-prefix-arg)
            (insert "#_")
            (setq i (1+ i)))))))

   ((equal '(4) arg)
    (save-mark-and-excursion
      (unless (and (equal (char-after) 40)
                   (equal (point) (line-beginning-position)))
        (beginning-of-defun)
        (if (equal "#_" (buffer-substring-no-properties (point) (+ 2 (point))))
            (delete-char 2)
          (insert "#_")))))))

;;; Packages
(use-package clojure-mode
  :bind
  (:map
   clojure-mode-map
   ("C-c C-i" . 'cider-inspect-last-result)
   ("C-#" . 'user/clojure-hash-comment))
  :init
  (add-hook 'clojure-mode-hook 'user/clojure-hide-comment)
  :config
  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  :custom
  (clojure-toplevel-inside-comment-form t)
  (clojure-indent-style 'always-indent))

(require 'the-clojure-highlight)

(use-package clj-refactor
  :pin "melpa-stable"
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (unbind-key "/" clj-refactor-map)
  (cljr-add-keybindings-with-prefix "C-c C-r")
  :custom
  (cljr-warn-on-eval t)
  (cljr-suppress-middleware-warnings t))

(use-package cider
  :pin "melpa-stable"
  :commands (cider-jack-in cider-jack-in-cljs cider-jack-in-clj&cljs)
  :bind
  (:map
   cider-mode-map
   ("C-!" . 'cider-read-and-eval)
   ("M-." . 'cider-find-var)
   :map
   cider-repl-mode-map
   ("M-," . 'cider-repl-handle-shortcut)
   ("C-," . 'cider-repl-handle-shortcut)
   ("<backspace>" . 'paredit-backward-delete))
  :config
  (unbind-key "M-." cider-mode-map)
  (unbind-key "C-c C-p" cider-mode-map)
  :init
  (add-hook 'cider--debug-mode-hook 'user/insert-mode)
  (setq-default cider-default-cljs-repl 'shadow)
  :custom
  (cider-font-lock-dynamically nil)
  (cider-font-lock-reader-conditionals nil)
  (cider-use-fringe-indicators t)
  (cider-prompt-for-symbol nil)
  (cider-save-file-on-load t)
  (cider-enhanced-cljs-completion-p t)
  (cider-offer-to-open-cljs-app-in-browser nil))

(provide 'the-clojure)
