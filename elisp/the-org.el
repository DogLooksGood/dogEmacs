(use-package org
  :commands (org-mode)
  :config
  (add-to-list 'org-modules 'org-tempo t)
  :custom
  (org-hide-emphasis-markers t)
  (org-export-html-postamble nil))

(provide 'the-org)
