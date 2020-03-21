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

(defun user/paredit-setup-for-terminal ()
  (define-key paredit-mode-map (kbd "M-[") nil))

(use-package paredit
  :hook
  ((paredit-mode . user/paredit-setup-for-terminal)
   (emacs-lisp-mode . paredit-mode)
   (clojure-mode . paredit-mode))
  :bind
  (:map
   paredit-mode-map
   ("M-[" . 'paredit-wrap-square)
   ("M-{" . 'paredit-wrap-curly)
   (";" . 'user/lisp-semicolon)))

(provide 'the-lisp)
