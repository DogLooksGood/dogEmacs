;;; Currently, enable meghanada will break the company for other major mode.

(make-variable-buffer-local 'company-backends)

(defun +java-setup ()
  (meghanada-mode t))

(use-package meghanada
  :hook
  (java-mode)
  :config
  (setq-local c-basic-offset 4)
  :bind
  (:map
   meghanada-mode-map
   ("C-c C-c C-r" . 'meghanada-exec-main)))

(provide 'init-java)
