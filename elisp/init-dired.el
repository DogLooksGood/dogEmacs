;;; -*- lexical-binding: t -*-

(setq dired-dwim-target t)
(bind-key "C-x C-d" 'dired)
(bind-key "q" 'kill-buffer-and-window dired-mode-map)
(bind-key "/" 'swiper dired-mode-map)
(bind-key "F" 'find-file dired-mode-map)

(use-package dired-hide-dotfiles
  :bind
  (:map
   dired-mode-map
   ("M-d" . 'dired-hide-dotfiles-mode)))

(use-package dired-sidebar
  :hook (dired-sidebar-mode . hl-line-mode)
  :bind
  (("C-M-S" . 'dired-sidebar-toggle-sidebar)
   :map
   dired-sidebar-mode-map
   ("q" . 'kill-buffer-and-window))
  :custom
  (dired-sidebar-subtree-line-prefix "  ")
  (dired-sidebar-theme 'ascii)
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-use-custom-font t))

(provide 'init-dired)
