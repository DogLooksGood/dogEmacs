(require 'm4d)

(m4d-global-mode 1)

(m4d-setup)

(add-to-list 'm4d-motion-mode-list 'ripgrep-search-mode)
(add-to-list 'm4d-motion-mode-list 'cider-repl-mode)
(add-to-list 'm4d-motion-mode-list 'vterm-mode)

(defun user/kill-window-or-quit ()
  (interactive)
  (condition-case e
      (delete-window)
    (error
     (save-buffers-kill-terminal))))

(m4d-normal-define-key
 '("<tab>" . user/normal-tab)
 '("TAB" . user/normal-tab)
 '("@" . hs-toggle-hiding))

(m4d-leader-define-key
 '("p" . counsel-projectile-find-file)
 '("G" . counsel-projectile-rg)
 '("g" . projectile-ripgrep)
 '("f" . find-file)
 '("a" . counsel-projectile-switch-project)
 '("Q" . pojectile-kill-buffers)
 '("d" . dired)
 '("k" . kill-buffer)
 '("l" . goto-line)
 '("h" . m4d-other-window)
 '("e" . m4d-eval-last-sexp)
 '("x" . m4d-eval-defun)
 '("w" . save-buffer)
 '("t" . transpose-sexps)
 '("i" . counsel-semantic-or-imenu)
 '("r" . sp-raise-sexp)
 '("s" . sp-split-sexp)
 '("u" . sp-splice-sexp)
 '("j" . sp-join-sexp)
 '("(" . sp-wrap-round)
 '("[" . sp-wrap-square)
 '("{" . sp-wrap-curly)
 '("v" . find-alternate-file)
 '("1" . delete-other-windows)
 '("2" . split-window-below)
 '("3" . split-window-right)
 '("o" . delete-other-windows)
 '("-" . split-window-below)
 '("\\" . split-window-right)
 '("0" . delete-window)
 '("b" . counsel-switch-buffer)
 '("'" . paredit-meta-doublequote)
 '("q" . projectile-kill-buffers)
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
   '("c&" . cider-jack-in-clj&cljs)
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

(m4d-leader-define-mode-key
 'markdown-mode
 '("ck" . markdown-insert-kbd))

(provide 'the-m4d)
