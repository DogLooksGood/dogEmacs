;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  setup for font, frame alpha, mode line and themes.

;; Transparency Setup
(defvar user/alpha nil)
(setq user/alpha 98)

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
    (set-frame-font "meslo lg m 9" t t)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family "Sarasa Mono SC" :size 32))))
  (user/set-font))

(setq underline-minimum-offset 0)

;;; Theme Setup
(setq x-underline-at-descent-line t)

;; Face tweaks
(custom-set-faces
 '(highlight-symbol-face ((t :underline "#668899")))
 '(hl-line ((t :underline "#5A5A5A" :overline "#5A5A5A")))
 '(fringe ((t :background nil)))
 '(yas-field-highlight-face ((t :box "#777"))))



(when (display-graphic-p)
;; (use-package berrys-theme
;;   :init
;;   (load-theme 'berrys t))
  (use-package zenburn-theme
    :init
    (load-theme 'zenburn t)))

;;; Mode Line Setup
(unless (display-graphic-p)
  (setq-default mode-line-format
                '(" "
                  (:eval (m4d-indicator))
                  " %l:%c "
                  " %b%* %e <%m>")))

;;; Run setup for future frames.

(defun user/new-frame-setup (frame)
  (select-frame frame)
  (user/set-alpha))

(when (display-graphic-p)
  (add-hook 'after-make-frame-functions 'user/new-frame-setup))

(provide 'the-look-and-feel)
