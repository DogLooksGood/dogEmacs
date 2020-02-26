;;; -*- lexical-binding: t -*-
;;; Completion

(require 'company)

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'conf-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook 'company-mode)

(setq company-frontends '(company-tng-frontend
                          company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))

(setq company-begin-commands
      '(self-insert-command
        user/insert-mode
        m4d-insert
        m4d-insert-after
        m4d-open-line
        m4d-open-line-up
        m4d-change
        kill-line
        paredit-backward-delete
        backward -delete-char
        backward-kill-word
        backward-kill-sexp))

(setq company-idle-delay 0.8
      company-minimum-prefix-length 4
      company-dabbrev-downcase nil
      company-abort-manual-when-too-short t
      company-require-match nil
      company-global-modes '(not dired-mode dired-sidebar-mode))

(with-eval-after-load "company"
  (progn
    (define-key company-mode-map (kbd "<tab>") 'user/insert-tab)
    (define-key company-active-map (kbd "}") 'company-select-next)
    (define-key company-active-map (kbd "{") 'company-select-previous)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "SPC") nil)))

(require 'company-template)
(with-eval-after-load "company-template"
  (progn
    (define-key company-template-nav-map (kbd "RET") 'company-template-forward-field)
    (define-key company-template-nav-map (kbd "TAB") nil)
    (define-key company-template-nav-map (kbd "<tab>") nil)))

(provide 'the-completion)
