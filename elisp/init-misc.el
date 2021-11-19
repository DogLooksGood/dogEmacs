(straight-use-package 'dumb-jump)
(straight-use-package 'pinentry)
(straight-use-package 'highlight-numbers)
(straight-use-package 'rainbow-mode)

(require 'dumb-jump)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; (add-hook 'prog-mode-hook 'eldoc-box-hover-mode)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

(pinentry-start)

(require 'colorful)
(add-hook 'prog-mode-hook 'colorful-mode)

(require 'htab)
(htab-mode 1)
(setq-default tab-line-format '((:eval (htab-indicator))))
(add-to-list 'htab-ignore-commands 'meow-minibuffer-quit)
(global-set-key (kbd "<XF86Forward>") #'htab-next-buffer)
(global-set-key (kbd "<XF86Back>") #'htab-prev-buffer)

(provide 'init-misc)
