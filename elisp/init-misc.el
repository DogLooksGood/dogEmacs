(straight-use-package 'dumb-jump)
(straight-use-package 'highlight-numbers)
(straight-use-package 'eldoc-overlay)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(autoload 'dump-jump-xref-active "dump-jump")
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

(require 'colorful)
(add-hook 'prog-mode-hook 'colorful-mode)

;; info
(require 'view)
(define-key Info-mode-map [remap scroll-up-command] #'View-scroll-half-page-forward)
(define-key Info-mode-map [remap scroll-down-command] #'View-scroll-half-page-backward)

(provide 'init-misc)
