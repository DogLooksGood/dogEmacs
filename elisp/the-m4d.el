(require 'm4d)

(m4d-global-mode 1)

(m4d-setup)

(add-to-list 'm4d-motion-escape-mode-list 'ripgrep-search-mode)
(add-to-list 'm4d-motion-mode-list 'cider-repl-mode)

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
 '("<tab>" . user/normal-tab)
 '("TAB" . user/normal-tab)
 '(">" . highlight-symbol-next)
 '("<" . highlight-symbol-prev))

(m4d-leader-define-key
 '("p" . projectile-find-file)
 '("g" . projectile-ripgrep)
 '("f" . find-file)
 '("P" . projectile-switch-project)
 '("d" . dired)
 '("k" .  kill-buffer)
 '("." . goto-line)
 '("e" . m4d-eval-last-sexp)
 '("x" . m4d-eval-defun)
 '("w" . save-buffer)
 '("t" . transpose-sexps)
 '("r" . paredit-raise-sexp)
 '("s" . paredit-splice-sexp)
 '("u" . paredit-split-sexp)
 '("(" . sp-wrap-round)
 '("[" . sp-wrap-square)
 '("{" . sp-wrap-curly)
 '("'" . paredit-meta-doublequote)
 '("q" . projectile-kill-buffers)
 '("o" . delete-other-windows)
 '("m" . magit-status))

(dolist (m '(clojure-mode clojurescript-mode clojurec-mode))
  (m4d-leader-define-mode-key
   m
   '("ce" . cider-eval-last-sexp)
   '("cc" . cider-eval-defun-at-point)
   '("cl" . cider-load-file)
   '("cn" . cider-eval-ns-form)
   '("cj" . cider-jack-in)
   '("cs" . cider-jack-in-cljs)
   '("ck" . cider-eval-buffer)
   '("cq" . cider-quit)
   '("cz" . cider-switch-to-repl-buffer)
   '(";" . user/clojure-hash-comment)))

(m4d-leader-define-mode-key
 'rust-mode
 '("cp" . rust-compile)
 '("cc" . rust-run)
 '("ct" . rust-test))

(m4d-leader-define-mode-key
 'org-mode
 '("ct" . org-todo))

(provide 'the-m4d)
