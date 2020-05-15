;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  mode line and themes.

(set-frame-parameter nil 'alpha '(100 . 100))

;;; Ensure the fonts is monospaced.
;;; 一二三四五六七八九十
;;; 11223344556677889900

(set-face-attribute 'default nil  :family "unifont" :height 120 :weight 'normal)
(set-face-attribute 'fixed-pitch nil  :family "unifont" :height 120 :weight 'normal)

(progn
  (use-package joker-theme
    :quelpa
    (joker-theme :repo "DogLooksGood/joker-theme" :fetcher github)
    ;; (joker-theme :fetcher file :path "~/develop/joker-theme")
    )
  (if +dumped-load-path
      (enable-theme 'joker)
    (load-theme 'joker t)))

;;; Mode Line Setup
(defun +simple-mode-line-render (left right)
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
(setq +mini-mode-line t)

(defun +mode-base-info (format-string)
  "Return formatted string if there's still enough space."
  (let ((s (format-mode-line format-string)))
    (when (or (not mini-modeline--msg)
              (> (window-width) (+ 12 (string-width s) (string-width mini-modeline--msg))))
      s)))

(use-package mini-modeline
  :quelpa
  (mini-modeline :repo "DogLooksGood/emacs-mini-modeline" :fetcher github)
  :init
  (setq mini-modeline-r-format '((:eval (+mode-base-info '("%l:%c %b %* %m" (vc-mode vc-mode))))
                                 ;; " ["
                                 ;; (:eval (number-to-string (point)))
                                 ;; "] "
                                 " "
                                 (:eval (when (featurep 'rime) (rime-lighter)))))
  (setq mini-modeline-l-format '((:eval (when (fboundp 'meow-indicator)
                                          (meow-indicator)))
                                 " "
                                 (:eval (mini-modeline-msg))))
  (setq mini-modeline-enhance-visual nil
        mini-modelineecho-duration 2)
  (mini-modeline-mode 1))

;; Only show window divider when there's more than one window.
(defun +toggle-window-divider-and-border ()
  (unless (string-match-p ".*-posframe\\*" (buffer-name (current-buffer)))
    (if (> (count-windows) 1)
        (progn
          (window-divider-mode 1))
      (progn
        (window-divider-mode -1)))))

(add-hook 'window-configuration-change-hook #'+toggle-window-divider-and-border)

(provide 'init-look-and-feel)
