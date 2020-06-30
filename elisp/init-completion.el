;;; -*- lexical-binding: t -*-
;;; Completion

(use-package company
  :bind
  (:map
   company-mode-map
   ("<tab>" . '+insert-tab)
   :map
   company-active-map
   ("}" . 'company-select-next)
   ("{" . 'company-select-previous)
   ("<tab>" . 'company-complete-selection)
   ("TAB" . 'company-complete-selection)
   ("RET")
   ("<return>")
   ("SPC")
   :map
   company-template-nav-map
   ("RET" . 'company-template-forward-field)
   ("<return>" . 'company-template-forward-field)
   ("TAB")
   ("<tab>"))
  :init
  (require 'company-template)
  :hook
  ((prog-mode . company-mode)
   (conf-mode . company-mode)
   (eshell-mode . company-mode))
  :custom
  (company-frontends '(company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-begin-commands
   '(self-insert-command
     meow-insert-before
     meow-insert-open
     meow-insert-after
     meow-insert-kill
     kill-line
     paredit-backward-delete
     backward -delete-char
     backward-kill-word
     backward-kill-sexp))
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 5)
  (company-dabbrev-downcase nil)
  (company-abort-manual-when-too-short t)
  (company-require-match nil)
  (company-global-modes '(not dired-mode dired-sidebar-mode)))

(use-package company-posframe
  :init
  (company-posframe-mode 1)
  :custom
  (company-posframe-quickhelp-delay nil)
  (company-posframe-show-metadata nil)
  (company-posframe-show-indicator nil))

(provide 'init-completion)
