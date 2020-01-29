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
 '("<tab>" . user/normal-tab)
 '("TAB" . user/normal-tab))

(m4d-leader-define-key
 '("fp" . counsel-projectile-find-file)
 '("fe" . counsel-projectile-rg)
 '("fg" . projectile-ripgrep)
 '("ff" . counsel-find-file)
 '("fo" . counsel-projectile-switch-project)
 '("fd" . counsel-dired)
 '("bb" . counsel-switch-buffer)
 '("bi" . counsel-ibuffer)
 '("bk" . kill-buffer)
 '("bn" . next-buffer)
 '("bp" . previous-buffer)
 '("gg" . goto-line)
 '("gt" . beginning-of-buffer)
 '("gb" . end-of-buffer)
 '("ee" . eval-last-sexp)
 '("ed" . eval-defun)
 '("eq" . save-buffers-kill-terminal)
 '("i" . ivy-resume)
 '("w" . save-buffer)
 '("t" . transpose-sexps)
 '("r" . paredit-raise-sexp)
 '("s" . paredit-splice-sexp)
 '("u" . paredit-split-sexp)
 '("(" . sp-wrap-round)
 '("[" . sp-wrap-square)
 '("{" . sp-wrap-curly)
 '("'" . paredit-meta-doublequote)
 '("xl" . downcase-dwim)
 '("xc" . capitalize-dwim)
 '("xu" . upcase-dwim)
 '("q" . user/kill-window-or-quit)
 '("o" . delete-other-windows)
 '("m" . magit-status))

(m4d-leader-define-mode-key
 'clojure-mode
 '("ce" . cider-eval-last-sexp)
 '("cc" . cider-eval-defun-at-point)
 '("cl" . cider-load-file)
 '("cn" . cider-eval-ns-form)
 '("cj" . cider-jack-in)
 '("cs" . cider-jack-in-clojurescript)
 '("ck" . cider-eval-buffer)
 '("cq" . cider-quit)
 '("cz" . cider-switch-to-repl-buffer)
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
