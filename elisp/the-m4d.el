(require 'm4d)

(m4d-global-mode 1)

(m4d-setup)

(add-to-list 'm4d-enable-mode-list 'cider-repl-mode)

(use-package key-chord
  :init
  (key-chord-define global-map ".," 'm4d-esc)
  (advice-add #'key-chord-mode :around #'user/make-silent)
  (key-chord-mode 1))

(defun user/kill-window-or-quit ()
  (interactive)
  (condition-case e
   (delete-window)
   (error
    (save-buffers-kill-terminal))))

(m4d-normal-define-key
 '("<tab>" . user/normal-tab))

(m4d-leader-define-key
 '("s" . paredit-split-sexp)
 '("r" . paredit-raise-sexp)
 '("e" . eval-last-sexp)
 '("x" . eval-buffer)
 '("f" . find-file)
 '("pg" . project-find-regexp)
 '("pf" . project-find-file)
 '("pe" . project-or-external-find-regexp)
 '("k" . kill-buffer)
 '("v" . find-alternate-file)
 '("g" . goto-line)
 '("b" . counsel-ibuffer)
 '("m" . magit-status)
 '("w" . save-buffer)
 '("q" . user/kill-window-or-quit)
 '("i" . back-to-indentation)
 '("(" . paredit-wrap-round)
 '("[" . paredit-wrap-square)
 '("{" . paredit-wrap-curly)
 '("o" . delete-other-windows)
 '("d" . dired))

(m4d-leader-define-mode-key
 'clojure-mode
 '("e" . cider-eval-last-sexp)
 '("x" . cider-load-file)
 '("cc" . cider-eval-defun-at-point)
 '("n" . cider-eval-ns-form)
 '("cj" . cider-jack-in)
 '("cs" . cider-jack-in-clojurescript)
 '("ck" . cider-eval-buffer)
 '(";" . user/clojure-hash-comment))

(m4d-leader-define-mode-key
 'rust-mode
 '("cp" . rust-compile)
 '("cc" . rust-run)
 '("ct" . rust-test))

(m4d-leader-define-mode-key
 'org-mode
 '("ct" . org-todo))

(provide 'the-m4d)
