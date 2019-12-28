(bind-key "C-x C-d" 'dired)

(bind-key "q" 'kill-buffer-and-window dired-mode-map)
(bind-key "/" 'swiper dired-mode-map)
(bind-key "F" 'find-file dired-mode-map)

(setq dired-listing-switches "-aBhl --group-directories-first")

(use-package dired-hide-dotfiles
  :init
  (bind-key "M-d" 'dired-hide-dotfiles-mode dired-mode-map))

(use-package dired-sidebar
  :bind
  (("C-M-S" . 'dired-sidebar-toggle-sidebar)
   :map
   dired-sidebar-mode-map
   ("q" . 'kill-buffer-and-window))
  :init
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode)
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(provide 'the-dired)
