;;; -*- lexical-binding: t -*-

;;; Parenthese management.

;;; For Lisp Dialects.

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

(defun +paredit-setup-for-terminal ()
  (define-key paredit-mode-map (kbd "M-[") nil))

(use-package paredit
  :hook
  (paredit-mode . +paredit-setup-for-terminal)
  ((emacs-lisp-mode clojure-mode) . paredit-mode)
  :bind
  (:map
   paredit-mode-map
   ("M-[" . 'paredit-wrap-square)
   ("M-{" . 'paredit-wrap-curly)
   (";" . '+lisp-semicolon)))

;;; For C like languages

(use-package smartparens
  :hook
  ((rust-mode go-mode java-mode rjsx-mode conf-mode snippet-mode json-mode css-mode web-mode html-mode cider-repl-mode-hook ) . 'smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-k" . 'sp-kill-hybrid-sexp)
        ("C-)" . 'sp-forward-slurp-sexp)
        ("C-}" . 'sp-forward-barf-sexp))
  :config
  (sp-with-modes
      '(rust-mode go-mode java-mode rjsx-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))
  (sp-with-modes
      '(rust-mode java-mode)
    (sp-local-pair "<" ">"))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil))

(provide 'init-paren)
