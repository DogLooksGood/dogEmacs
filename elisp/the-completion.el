;;; -*- lexical-binding: t -*-
;;; Completion

(use-package company
  :ensure t
  :bind
  (:map company-mode-map
   ("<tab>" . 'user/insert-tab)
   :map company-active-map
   ("}" . 'company-select-next)
   ("{" . 'company-select-previous)
   ("[" . 'company-show-doc-buffer))
  :config
  (unbind-key "RET" company-active-map)
  (unbind-key "<return>" company-active-map)
  (unbind-key "TAB" company-active-map)
  (unbind-key "SPC" company-active-map)
  :init
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
        backward-delete-char
        backward-kill-word
        backward-kill-sexp))
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 4
        company-dabbrev-downcase nil
        company-abort-manual-when-too-short t
        company-require-match nil
        company-global-modes '(not dired-mode dired-sidebar-mode))
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode)
  (add-hook 'conf-mode-hook 'company-mode)
  (add-hook 'eshell-mode-hook 'company-mode))

(provide 'the-completion)
