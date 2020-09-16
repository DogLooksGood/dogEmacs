;;; -*- lexical-binding: t -*-

;;; Parenthese management.

;;; Use paredit for lisp
(defun +lisp-semicolon ()
  "Will insert a semicolon if we are at the beginning of the line,
otherwise will insert a colon."
  (interactive)
  (if (and (or (nth 3 (syntax-ppss))
               (nth 4 (syntax-ppss))
               (equal (point) (line-beginning-position)))
           (not (equal 'cider-repl-mode major-mode)))
      (insert ";")
    (insert ":")))

(use-package paredit
  :hook
  ((emacs-lisp-mode clojure-mode) . paredit-mode)
  :bind
  (:map
   paredit-mode-map
   ("M-[" . 'paredit-wrap-square)
   ("M-{" . 'paredit-wrap-curly)
   (";" . '+lisp-semicolon)))

;;; Use electric-pair mode for elixir and python
(mapcar
 (lambda (m)
   (add-hook (intern (concat (symbol-name m) "-hook")) 'electric-pair-local-mode))
 '(elixir-mode python-mode))


;;; Use smartparens for other modes
(use-package smartparens
  :hook
  ((rust-mode go-mode java-mode js-mode conf-mode snippet-mode json-mode css-mode web-mode html-mode cider-repl-mode) . 'smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-k" . 'sp-kill-hybrid-sexp)
        ("C-)" . 'sp-slurp-hybrid-sexp)
        ("C-}" . 'sp-forward-barf-sexp)
        ("C-M-t" . 'sp-transpose-sexp)
        ("M-r" . 'sp-raise-sexp))
  :config
  (sp-with-modes
      '(elixir-mode)
    (sp-local-pair "\"" "\""))
  (sp-with-modes
      '(rust-mode go-mode java-mode js-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (sp-with-modes
      '(rust-mode java-mode)
    (sp-local-pair "<" ">"))
  (sp-with-modes
      '(cider-repl-mode)
    (sp-local-pair "'" nil :actions nil))
  :custom
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil))

(provide 'init-paren)
