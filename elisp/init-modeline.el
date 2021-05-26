;; Mode Line  -*- lexical-binding: t; -*-

(setq-default header-line-format nil)

(setq-default mode-line-format
              '((:eval (meow-indicator))
                (:eval (rime-lighter))
                (:eval (+smart-file-name-cached))
                " Ln %l Col %C "
                mode-name
                " "
                (vc-mode vc-mode)))

(provide 'init-modeline)
