(bind-key "C-x C-d" 'dired)

(bind-key "q" 'kill-buffer-and-window dired-mode-map)
(bind-key "/" 'swiper dired-mode-map)
(bind-key "{" 'scroll-down-command dired-mode-map)
(bind-key "}" 'scroll-up-command dired-mode-map)
(bind-key "[" 'beginning-of-buffer dired-mode-map)
(bind-key "]" 'end-of-buffer dired-mode-map)
(bind-key "F" 'find-file dired-mode-map)

(setq dired-listing-switches "-aBhl --group-directories-first")

(use-package dired-hide-dotfiles
  :init
  (bind-key "M-d" 'dired-hide-dotfiles-mode dired-mode-map))

(use-package dired-sidebar
  :bind
  (("C-S-T" . 'dired-sidebar-toggle-sidebar)
   :map
   dired-sidebar-mode-map
   ("{" . 'scroll-down-command)
   ("}" . 'scroll-up-command)
   ("[" . 'beginning-of-buffer)
   ("]" . 'end-of-buffer)
   ("q" . 'kill-buffer-and-window))
  :init
  (add-hook 'dired-sidebar-mode-hook 'hl-line-mode)
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'none)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(provide 'the-dired)
