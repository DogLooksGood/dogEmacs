;;; -*- lexical-binding: t -*-

(defun +meow-setup ()
  (meow-normal-define-key
   '("/" . swiper))

  (meow-leader-define-key
   '("L" . display-line-numbers-mode)
   '("D" . docker)
   '("G" . diff-hl-mode)
   '(";" . dired-sidebar-toggle-sidebar)
   '("k" . kill-buffer)
   '("h" . meow-keypad-start)
   '("l" . goto-line)
   '("w" . ace-swap-window)
   '("z" . +eval-and-bound-to-c-z)
   '("i" . counsel-imenu)
   '("n" . dumb-jump-go)
   '("j" . sp-join-sexp)
   '("(" . sp-wrap-round)
   '("[" . sp-wrap-square)
   '("{" . sp-wrap-curly)
   '("o" . ace-window)
   '("a" . delete-other-windows)
   '("-" . split-window-below)
   '("/" . swiper)
   '("\\" . split-window-right)
   '("'" . paredit-meta-doublequote)
   '("*" . ivy-pass)
   '("#" . deft)
   '("m" . magit-status)
   '("M" . magit-blame)
   '("p" . projectile-find-file)
   '("b" . switch-to-buffer)
   '("g" . projectile-ripgrep)
   '("f" . find-file)
   '("F" . find-file-literally)
   '("y" . tiny-expand)
   '("z" . winner-undo)
   '("!" . +open-work-log)
   '("." . highlight-symbol-at-point)
   '("," . unhighlight-regexp)
   '("|" . +toggle-theme)
   '("'" . universal-argument)))

(use-package meow
  :quelpa
  ;; (meow :repo "DogLooksGood/meow" :fetcher github)
  (meow :fetcher file :path "~/source/meow")
  :bind
  (:map meow-insert-state-keymap
   ("<tab>" . '+insert-tab))
  :config
  (+meow-setup)
  (meow-global-mode 1)
  (add-to-list 'meow-normal-state-mode-list 'restclient-mode)
  (add-to-list 'meow-normal-state-mode-list 'slack-message-buffer-mode)
  (add-to-list 'meow-normal-state-mode-list 'slack-thread-message-buffer-mode)
  (add-to-list 'meow-normal-state-mode-list 'messages-buffer-mode)
  (add-to-list 'meow-normal-state-mode-list 'alchemist-iex-mode)
  (add-to-list 'meow-normal-state-mode-list 'inf-iex-mode)
  :custom
  (meow-layout 'dvp)
  (meow-esc-delay 0.001))

(provide 'init-meow)
