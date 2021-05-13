(straight-use-package 'writeroom-mode)
(straight-use-package 'dumb-jump)
(straight-use-package 'frames-only-mode)
(straight-use-package 'vterm)
(straight-use-package 'pinentry)

(+pdump-packages 'writeroom-mode)
(+pdump-packages 'dumb-jump)
(+pdump-packages 'frames-only-mode)

;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(with-eval-after-load "writeroom-mode"
  (setq writeroom-width 120
        writeroom-extra-line-spacing 5
        writeroom-fullscreen-effect 'fullboth))

(autoload #'writeroom-mode "writeroom-mode" nil t)

(require 'dumb-jump)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(pinentry-start)

(provide 'init-misc)
