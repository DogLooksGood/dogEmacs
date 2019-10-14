(use-package all-the-icons)

(use-package treemacs
  :bind
  (("C-S-T" . 'treemacs)
   :map
   treemacs-mode-map
   ("{" . 'scroll-down-command)
   ("}" . 'scroll-up-command)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer))
  :init
  (setq treemacs-no-png-images t))

(provide 'the-treemacs)
