;;; -*- lexical-binding: t -*-
;;; Look And Feels
;;  setup for font, frame alpha, mode line and themes.

;; Transparency Setup
(when (display-graphic-p)
  (defun user/set-alpha (&rest args)
    (let ((alpha (or (car args) 100)))
      (set-frame-parameter (selected-frame) 'alpha (cons alpha alpha))))

  (user/set-alpha 100))

;;; Font Setup
;; sample text:
;;   | 中英文等宽的字体 |
;;   | Mixed mono font  |
;; get this script from cnfont

(when (display-graphic-p)
  (defun user/set-font (&rest args)
    (set-frame-font "monospace-9" t t)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font)
       charset
       (font-spec :family "wenquanyi micro hei mono" :size 32))))
  (user/set-font))

;;; Theme Setup
;; Face tweaks
(custom-set-faces
 '(highlight-symbol-face ((t :underline "#668899")))
 '(fringe ((t :background nil)))
 '(yas-field-highlight-face ((t :box "#777")))
 '(form-feed-line ((t :strike-through "#666"))))

;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t))

;;; A beautiful and clean white theme.
(use-package berrys-theme
  :init
  (load-theme 'berrys t))

;;; Mode Line Setup

(setq-default mode-line-format nil)

;;; Run setup for future frames.

(defun user/new-frame-setup (frame)
  (select-frame frame)
  (user/set-alpha)
  (user/set-font))

(when (display-grayscale-p)
  (add-hook 'after-make-frame-functions 'user/new-frame-setup))

(provide 'the-look-and-feel)
