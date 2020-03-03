;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  mode line and themes.

;; (set-frame-parameter nil 'alpha '(100 . 90))

;;; Ensure the fonts is monospaced.
;;; 一二三四五六七八九十
;;; 11223344556677889900

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

(defun user/mode-base-info (format-string)
  "Return formatted string if there's still enough space."
  (let ((s (format-mode-line format-string)))
    (when (or (not mini-modeline--msg)
              (> (window-width) (+ 12 (string-width s) (string-width mini-modeline--msg))))
      s)))

(setq mini-modeline-r-format '((:eval (user/mode-base-info '("%l:%c %b %* %m" (vc-mode vc-mode))))
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
