;;; -*- lexical-binding: t -*-

(defun +meow-setup ()
  (meow-normal-define-key
   '("S" . kmacro-start-macro)
   '("E" . kmacro-end-macro)
   '("A" . apply-macro-to-region-lines)
   '("M" . kmacro-call-macro))

  (meow-leader-define-key
   '("SPC" . avy-goto-line)
   '("k" . kill-buffer)
   '("l" . goto-line)
   '("h" . ace-window)
   '("w" . ace-swap-window)
   '("z" . +eval-and-bound-to-c-z)
   '("i" . counsel-imenu)
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
   '("*" . ivy-pass)
   '("#" . deft)
   '("m" . magit-status)
   '("b" . magit-blame)
   '("p" . projectile-find-file)
   '("b" . counsel-switch-buffer)
   '("g" . projectile-ripgrep)
   '("f" . find-file)
   '("F" . find-file-literally)
   '("y" . tiny-expand)
   '("a" . emamux:send-region)
   '("!" . +open-work-log)
   '("$" . +send-clojure-ns-form)
   '("." . highlight-symbol-at-point)
   '("," . unhighlight-regexp)))

(use-package meow
  :quelpa
  (meow :repo "DogLooksGood/meow" :fetcher github)
  ;; (meow :fetcher file :path "~/Projects/meow")
  :config
  (+meow-setup)
  (meow-global-mode 1)
  (add-to-list 'meow-normal-state-mode-list 'restclient-mode)
  :custom
  (meow-layout 'dvp))

(provide 'init-meow)
