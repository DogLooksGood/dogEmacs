(straight-use-package 'smartparens)
(straight-use-package 'anzu)

(autoload #'anzu-query-replace "anzu" nil t)
(autoload #'anzu-query-replace-regexp "anzu" nil t)

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

(setq sp-autowrap-region nil)

(autoload #'smartparens-mode "smartparens" nil t)

(defun +lisp-semicolon ()
  "Will insert a semicolon if we are at the beginning of the line,
otherwise will insert a colon."
  (interactive)
  (if (or (+in-comment-p)
          (+in-string-p)
          (equal (point) (line-beginning-position)))
      (call-interactively #'sp-comment)
    (insert ":")))

(with-eval-after-load "smartparens"
  (define-key smartparens-mode-map (kbd "M-r") #'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "M-s") #'sp-unwrap-sexp)
  (when window-system
    (define-key smartparens-mode-map (kbd "M-[") #'sp-wrap-square))
  (define-key smartparens-mode-map (kbd "M-{") #'sp-wrap-curly)
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp)

  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil)

  (sp-with-modes '(lisp-mode emacs-lisp-mode clojure-mode lisp-interaction-mode)
    (sp-local-pair "'" nil :actions nil)))

(dolist (h '(c-mode-hook))
  (add-hook h 'smartparens-mode))

(dolist (h '(lisp-mode-hook lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook h 'smartparens-mode)
  (add-hook h 'smartparens-strict-mode))

(define-key lisp-mode-map (kbd ";") '+lisp-semicolon)

(provide 'init-edit)
