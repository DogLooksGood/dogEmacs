;;; Currently, enable meghanada will break the company for other major mode.

(make-variable-buffer-local 'company-backends)

(defun user/java-setup ()
  (meghanada-mode t)
  (flycheck-mode 1)
  (smartparens-mode)
  (sp-local-pair 'java-mode "<" ">")
  (setq c-basic-offset 4))

(use-package meghanada
  :bind
  (:map meghanada-mode-map
        ("C-c C-c C-r" . 'meghanada-exec-main))
  :init
  (setq flycheck-idle-change-delay 2)
  (add-hook 'java-mode-hook 'user/java-setup))

(provide 'the-java)
