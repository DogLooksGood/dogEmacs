(require 'm4d)

(m4d-global-mode 1)

(m4d-setup)

(add-to-list 'm4d-enable-mode-list 'cider-repl-mode)

(m4d-normal-define-key
 '("<tab>" . user/normal-tab))

(m4d-leader-define-key
 '("e" . eval-last-sexp)
 '("x" . eval-buffer)
 '("f" . find-file)
 '("p" . counsel-projectile-find-file)
 '("s" . counsel-projectile-rg)
 '("v" . find-alternate-file)
 '("r" . projectile-ripgrep)
 '("g" . goto-line)
 '("b" . counsel-ibuffer)
 '("m" . magit-status)
 '("w" . save-buffer)
 '("q" . save-buffers-kill-terminal)
 '("i" . back-to-indentation)
 '("(" . paredit-wrap-round)
 '("[" . paredit-wrap-square)
 '("{" . paredit-wrap-curly)
 '("d" . dired))

(m4d-leader-define-mode-key
 'clojure-mode
 '("e" . cider-eval-last-sexp)
 '("x" . cider-load-file)
 '(";" . user/clojure-hash-comment)
 '("cc" . cider-eval-defun-at-point)
 '("n" . cider-eval-ns-form)
 '("cj" . cider-jack-in)
 '("cs" . cider-jack-in-clojurescript)
 '("ck" . cider-eval-buffer))

(m4d-leader-define-mode-key
 'rust-mode
 '("cp" . rust-compile)
 '("cc" . rust-run)
 '("ct" . rust-test))

(m4d-leader-define-mode-key
 'org-mode
 '("ct" . org-todo))

(provide 'the-m4d)
