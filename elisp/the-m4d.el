(require 'm4d)

(m4d-global-mode 1)

(m4d-setup)

(defun user/kill-window-or-quit ()
  (interactive)
  (condition-case e
      (delete-window)
    (error
     (save-buffers-kill-terminal))))

;;; In terminal we can use C-u to escape,
;;; ESC hack doesn't play well in mosh/ssh.
(unless (display-graphic-p)
  (global-set-key (kbd "C-x C-x") 'execute-extended-command)
  (define-key m4d-keypad-keymap (kbd "C-u") 'm4d-escape-or-normal-modal)
  (define-key isearch-mode-map (kbd "C-u") 'isearch-abort)
  (global-set-key (kbd "C-u") 'm4d-global-esc)
  (define-key m4d-insert-keymap (kbd "C-u") 'm4d-escape-or-normal-modal)
  (define-key m4d-normal-keymap (kbd "C-u") 'm4d-last-buffer))

(m4d-normal-define-key
 '("C" . hs-toggle-hiding)
 '("<tab>" . back-to-indentation)
 '("TAB" . back-to-indentation))

(m4d-leader-define-key
 '("," . highlight-symbol-prev)
 '("." . highlight-symbol-next)
 '("k" . kill-buffer)
 '("l" . goto-line)
 '("h" . m4d-other-window)
 '("e" . m4d-eval-last-sexp)
 '("z" . m4d-eval-defun)
 '("t" . transpose-sexps)
 '("i" . counsel-imenu)
 '("r" . sp-raise-sexp)
 '("s" . sp-split-sexp)
 '("u" . sp-splice-sexp)
 '("n" . dumb-jump-go)
 '("j" . sp-join-sexp)
 '("(" . sp-wrap-round)
 '("[" . sp-wrap-square)
 '("{" . sp-wrap-curly)
 '("o" . delete-other-windows)
 '("-" . split-window-below)
 '("/" . swiper)
 '("\\" . split-window-right)
 '("'" . paredit-meta-doublequote)
 '("#" . deft)
 '("q" . m4d-quit)
 '("m" . magit-status)
 '("b" . magit-blame)
 '("p" . projectile-find-file)
 '("b" . counsel-switch-buffer)
 '("g" . projectile-ripgrep)
 '("f" . find-file)
 '("F" . find-file-literally))

(dolist (m '(clojure-mode clojurescript-mode clojurec-mode))
  (m4d-leader-define-mode-key
   m
   '(";" . user/clojure-hash-comment)))

(m4d-leader-define-mode-key
 'emacs-lisp-mode
 '("RET" . eval-buffer)
 '(";" . eval-expression))

(m4d-leader-define-mode-key
 'cider-repl-mode
 '("cq" . cider-quit)
 '("cz" . cider-switch-to-last-clojure-buffer))

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
