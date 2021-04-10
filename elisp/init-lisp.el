;;; -*- lexical-binding: t -*-

(straight-use-package '(paredit :type git :host github :repo "emacsmirror/paredit"))

(+pdump-packages 'paredit)

(defun +lisp-semicolon ()
  "Will insert a semicolon if we are at the beginning of the line,
otherwise will insert a colon."
  (interactive)
  (if (or (+in-comment-p)
          (+in-string-p)
          (equal (point) (line-beginning-position)))
      (call-interactively #'self-insert-command)
    (insert ":")))

;;; paredit

(autoload #'paredit-mode "paredit")

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)

(with-eval-after-load "paredit"
  (define-key paredit-mode-map (kbd ";") '+lisp-semicolon))

(provide 'init-lisp)
