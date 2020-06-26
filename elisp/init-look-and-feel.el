;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  mode line and themes.

(set-frame-parameter nil 'alpha '(100 . 100))

(require 'joker-light-theme)
(require 'joker-theme)
(load-theme 'joker t)

(let ((font "unifont-11"))
  (add-to-list 'default-frame-alist (cons 'font font))
  (set-face-attribute 'fixed-pitch nil :family "unifont")
  (set-frame-font font nil t))

(defun +smart-file-name ()
  (if vc-mode
      (magit-file-relative-name)
    (buffer-name)))

(defun +project-name ()
  (if vc-mode
      (format "  { %s }" (vc-root-dir))
    ""))

;;; title line setup
(setq-default frame-title-format
              '("Emacs"
                (:eval (+project-name))))

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
  (setq mini-modeline-r-format '("%l:%c "
                                 (:eval (+smart-file-name))
                                 " %* %m"
                                 (vc-mode vc-mode)
                                 " "
                                 (:eval (when (featurep 'rime) (rime-lighter)))))
  (setq mini-modeline-l-format '((:eval (when (featurep 'meow) (meow-indicator)))
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
