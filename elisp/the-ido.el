
(use-package ido
  :bind
  (:map
   ido-common-completion-map
   ("{" . 'ido-prev-match)
   ("}" . 'ido-next-match))
  :init
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido-completing-read+
  :init
  (ido-ubiquitous-mode 1))

(use-package smex
  :bind
  ("M-x" . smex))

(provide 'the-ido)
