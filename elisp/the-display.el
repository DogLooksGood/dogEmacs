(use-package form-feed
  :ensure t
  :custom-face
  (form-feed-line ((t (:strike-through "#666"))))
  :init
  (add-hook 'prog-mode-hook #'form-feed-mode)
  :config
  (set-face-attribute 'form-feed-line nil :strike-through "#666"))

(provide 'the-display)
