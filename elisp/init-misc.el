(straight-use-package 'dumb-jump)
(straight-use-package 'highlight-numbers)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(autoload 'dump-jump-xref-active "dump-jump")
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;; ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry

(require 'colorful)
(add-hook 'prog-mode-hook 'colorful-mode)

;; (require 'htab)
;; (htab-global-mode 1)
;; (add-to-list 'htab-ignore-commands 'meow-minibuffer-quit)
;; (global-set-key (kbd "<XF86Forward>") #'htab-next-buffer)
;; (global-set-key (kbd "<XF86Back>") #'htab-prev-buffer)

(provide 'init-misc)
