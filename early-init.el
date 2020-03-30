(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (file-exists-p (expand-file-name "~/.emacs.d/emacs.pdmp"))
  (setq package-enable-at-startup nil
        file-name-handler-alist nil
        auto-window-vscroll nil))
