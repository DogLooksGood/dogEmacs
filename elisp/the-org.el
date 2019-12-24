(defun user/org-mode-setup ()
  (org-bullets-mode t))

(use-package org
  :commands (org-mode)
  :config
  (unbind-key "M-h" org-mode-map)
  (unbind-key "M-a" org-mode-map)
  (setq org-hide-emphasis-markers t)
  (setq org-export-html-postamble nil))

(use-package org-bullets
  :init
  (setq org-ellipsis "⤵")
  (add-hook 'org-mode-hook 'user/org-mode-setup))

(provide 'the-org)
