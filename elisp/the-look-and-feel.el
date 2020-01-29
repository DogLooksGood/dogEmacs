;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  setup for font, frame alpha, mode line and themes.

;; Transparency Setup
(defvar user/alpha nil)
(setq user/alpha 100)

(when (display-graphic-p)
  (defun user/set-alpha ()
    (set-frame-parameter (selected-frame) 'alpha (cons user/alpha user/alpha)))
  (user/set-alpha))

;;; Font Setup
;; sample text:
;;   | 中英文等宽的字体 |
;;   | Mixed monospace  |
;; get this script from cnfont

(when (display-graphic-p)
  (defun user/set-font (&rest args)
    (set-frame-font "cascadia mono-9" t t)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family "Sarasa Mono SC" :size 30))))
  (user/set-font))

(setq underline-minimum-offset 0)

;;; Theme Setup
(setq x-underline-at-descent-line t)

;; Face tweaks
(custom-set-faces
 '(highlight-symbol-face ((t :underline "#668899")))
 '(hl-line ((t :underline "#353535" :overline "#353535")))
 '(fringe ((t :background nil)))
 '(yas-field-highlight-face ((t :box "#777")))
 '(window-divider ((t :foreground "#3F3F3F"))))

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

(when (display-graphic-p)
    (setq-default mode-line-format nil))

;;; title line setup
(setq-default frame-title-format
              '("["
                (:eval
                 (when vc-mode
                   (replace-regexp-in-string "^ Git" " " vc-mode)))
                "]"
                " %b%* %e <%m>"))

(use-package mini-modeline
  :quelpa (mini-modeline :repo "kiennq/emacs-mini-modeline" :fetcher github)
  :config
  (setq mini-modeline-r-format '("%l:%c  %b%* %e %m "
                                 (:eval (m4d-indicator))))
  (setq mini-modeline-l-format '((:eval (mini-modeline-msg))))
  (setq mini-modeline-enhance-visual nil
        mini-modeline-echo-duration 2)
  (mini-modeline-mode t))

;;; Run setup for future frames.

(defun user/new-frame-setup (frame)
  (select-frame frame)
  (user/set-font)
  (user/set-alpha))

(when (display-graphic-p)
  (add-hook 'after-make-frame-functions 'user/new-frame-setup))

(provide 'the-look-and-feel)
