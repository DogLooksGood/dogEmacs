(use-package popwin
  :config
  (push '(direx:direx-mode :position left :width 35 :dedicated t)
        popwin:special-display-config)
  :init
  (popwin-mode 1))

(use-package direx
  :bind
  (("C-S-T" . 'direx-project:jump-to-project-root-other-window)))

(provide 'the-direx)
