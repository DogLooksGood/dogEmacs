(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; | 中英文等宽的字体 |
;;; | Mixed monospace  |
(set-face-attribute 'default nil  :family "Unifont" :height 120 :weight 'normal)

(when (file-exists-p (expand-file-name "~/.emacs.d/emacs.pdmp"))
  (setq package-enable-at-startup nil
        file-name-handler-alist nil
        message-log-max 16384
        gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        auto-window-vscroll nil))
