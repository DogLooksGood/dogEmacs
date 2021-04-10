(straight-use-package 'writeroom-mode)

(+pdump-packages 'writeroom-mode)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(with-eval-after-load "writeroom-mode"
  (setq writeroom-width 120
        writeroom-extra-line-spacing 5
        writeroom-fullscreen-effect 'fullboth))

(autoload #'writeroom-mode "writeroom-mode" nil t)

(global-set-key (kbd "<f8>") #'writeroom-mode)

(provide 'init-misc)
