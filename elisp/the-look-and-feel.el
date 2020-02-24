;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  setup for font, mode line and themes.

(defvar user/fonts nil)
(setq user/fonts '("unifont" "iosevka"))

(defun user/select-font ()
  (interactive)
  (let ((font (completing-read "Set font: " user/fonts)))
    (set-face-attribute 'default nil  :family font :height 120 :weight 'normal)))

(global-set-key (kbd "C-S-f") 'user/select-font)

(progn
  (require 'joker-theme)
  (if user/dumped-load-path
      (enable-theme 'joker)
    (load-theme 'joker t)))

;;; Mode Line Setup
(defun user/simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds " available-width) left right)))

;;; title line setup
(setq-default frame-title-format
              '("["
                (:eval
                 (when vc-mode vc-mode))
                " ]"
                " %b%* %e <%m>"))

;;; If we want hide the mode line
(setq user/mini-mode-line t)

(use-package mini-modeline
  :quelpa (mini-modeline :repo "DogLooksGood/emacs-mini-modeline" :fetcher github))

(setq mini-modeline-r-format '("%l:%c"
                               (vc-mode vc-mode)
                               " %b %*%e %m"
                               (:eval (when (fboundp 'rime-lighter)
                                        (rime-lighter)))))
(setq mini-modeline-l-format '((:eval (m4d-indicator))
                               " "
                               (:eval (mini-modeline-msg))))
(setq mini-modeline-enhance-visual nil
      mini-modelineecho-duration 2)
(mini-modeline-mode t)

;; Only show window divider when there's more than one window.
(defun user/toggle-window-divider-and-border ()
  (if (> (count-windows) 1)
      (progn
        (window-divider-mode 1))
    (progn
      (window-divider-mode -1))))

(window-divider-mode -1)
(add-hook 'window-configuration-change-hook #'user/toggle-window-divider-and-border)

(provide 'the-look-and-feel)
