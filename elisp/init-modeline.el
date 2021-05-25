;; Mode Line  -*- lexical-binding: t; -*-

(setq-default mode-line-format
              '((:eval (meow-indicator))
                (:eval (rime-lighter))
                " %l:%c "
                (:eval (+smart-file-name-cached))
                " "
                mode-name
                " "
                (vc-mode vc-mode)))

(provide 'init-modeline)
