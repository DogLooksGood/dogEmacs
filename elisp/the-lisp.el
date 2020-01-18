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
  :ensure t
  :bind
  (:map
   paredit-mode-map
   ("M-[" . 'paredit-wrap-square)
   ("M-{" . 'paredit-wrap-curly)
   (";" . 'user/lisp-semicolon))
  :init
  (add-hook 'paredit-mode-hook 'user/paredit-setup-for-terminal)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(provide 'the-lisp)
