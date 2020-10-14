;;; -*- lexical-binding: t -*-

(setq dired-dwim-target t)
(bind-key "C-x C-d" 'dired)
(bind-key "q" 'kill-buffer-and-window dired-mode-map)
(bind-key "F" 'find-file dired-mode-map)
(bind-key "TAB" 'dired-subtree-toggle dired-mode-map)
(bind-key "," 'dired-subtree-up dired-mode-map)

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls" dired-use-ls-dired t))

(setq dired-listing-switches "-lAXGh --group-directories-first")

(use-package dired-hide-dotfiles
  :bind
  (:map
   dired-mode-map
   ("M-d" . 'dired-hide-dotfiles-mode)))

(use-package dired-sidebar
  :bind
  (("C-|" . 'dired-sidebar-toggle-sidebar)
   :map
   dired-sidebar-mode-map
   ("q" . 'kill-buffer-and-window))
  :custom
  (dired-sidebar-theme (if +use-icons 'icons 'none))
  (dired-sidebar-subtree-line-prefix "  ")
  (dired-sidebar-use-term-integration t)
  (dired-sidebar-should-follow-file nil)
  (dired-sidebar-follow-file-idle-delay 0.25)
  (dired-sidebar-use-custom-font t)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

(when +use-icons
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(provide 'init-dired)
