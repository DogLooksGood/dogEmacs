(straight-use-package 'dumb-jump)
(straight-use-package 'pinentry)
(straight-use-package 'highlight-numbers)
(straight-use-package 'eldoc-box)

(require 'dumb-jump)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'eldoc-box-hover-mode)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

(pinentry-start)

(provide 'init-misc)
