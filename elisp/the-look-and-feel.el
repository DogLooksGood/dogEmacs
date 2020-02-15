;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  setup for font, mode line and themes.

;;; Font Setup
;; sample text:
;;   | 中英文等宽的字体 |
;;   | Mixed monospace  |
;; get this script from cnfont

(add-to-list 'default-frame-alist '(font . "Unifont 16"))
;; (set-frame-font "unifont 16" t t)

(setq underline-minimum-offset 0)

;;; Theme Setup
(setq x-underline-at-descent-line t)

(progn
  (require 'joker-theme)
  (setq visible-cursor nil)
  (load-theme 'joker t))

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
  :quelpa (mini-modeline :repo "kiennq/emacs-mini-modeline" :fetcher github)
  :config
  (if (and user/mini-mode-line (display-graphic-p))
      (progn
        (setq mini-modeline-r-format '("%l:%c" (vc-mode vc-mode) " %b%* %e %m"))
        (setq mini-modeline-l-format '((:eval (m4d-indicator))
                                       " "
                                       (:eval (mini-modeline-msg))))
        (setq mini-modeline-enhance-visual nil
              mini-modeline-echo-duration 2)
        (mini-modeline-mode t))
    (progn
      (setq mini-modeline-r-format '())
      (setq mini-modeline-l-format '((:eval (m4d-indicator))
                                     " "
                                     (:eval (mini-modeline-msg))))
      (setq mini-modeline-enhance-visual nil
            mini-modeline-echo-duration 2)
      (mini-modeline-mode t)
      (setq-default mode-line-format
              '((:eval (user/simple-mode-line-render
                        (format-mode-line '())
                        (format-mode-line '(" %l:%c %b%*"
                                            (vc-mode vc-mode)
                                            " %m")))))))))

;; Only show window divider when there's more than one window.
(defun user/toggle-window-divider-and-border ()
  (if (> (count-windows) 1)
      (progn
        (window-divider-mode 1))
    (progn
      (window-divider-mode -1))))

(when (and user/mini-mode-line (display-graphic-p))
  (window-divider-mode -1)
  (add-hook 'window-configuration-change-hook #'user/toggle-window-divider-and-border))

(provide 'the-look-and-feel)
