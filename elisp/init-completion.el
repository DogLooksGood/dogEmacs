;;; -*- lexical-binding: t -*-
;;; Completion

(use-package company
  :hook (company-mode . company-tng-mode)
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
  (company-tng-auto-configure nil)
  (company-frontends '(company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0.005)
  (company-tooltip-limit 7)
  (company-tooltip-align-annotations t)
  (company-tooltip-offset-display 'lines)
  (company-tooltip-width-grow-only t)
  (company-tooltip-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-dabbrev-downcase nil)
  (company-abort-manual-when-too-short t)
  (company-require-match nil)
  (company-global-modes '(not dired-mode dired-sidebar-mode))
  (company-tooltip-margin 0))

(unless +use-icons
  (use-package company-posframe
    :init
    (company-posframe-mode 1)
    :custom
    (company-posframe-quickhelp-delay nil)
    (company-posframe-show-indicator nil)
    (company-posframe-show-metadata nil)))

(when +use-icons
  (use-package company-box
    :hook (company-mode . company-box-mode)))

(use-package company-prescient
  :init
  (company-prescient-mode 1))

(provide 'init-completion)
