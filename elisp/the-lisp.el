(defun user/lisp-semicolon ()
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
  :ensure t
  :bind
  (:map
   paredit-mode-map
   ("C-(" . 'paredit-forward-barf-sexp)
   ("M-[" . 'paredit-wrap-square)
   ("M-{" . 'paredit-wrap-curly)
   (";" . 'user/lisp-semicolon))
  :config
  (unbind-key "C-{" paredit-mode-map)
  (unbind-key "C-}" paredit-mode-map)
  (unbind-key "M-q" paredit-mode-map)
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(provide 'the-lisp)
