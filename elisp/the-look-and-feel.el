;;; Look And Feels
;;  setup for font, frame alpha and themes.

;; Transparency Setup
(progn
  (defun user/set-alpha (&rest args)
    (let ((alpha (or (car args) 100)))
      (set-frame-parameter (selected-frame) 'alpha (cons alpha alpha))))

  (user/set-alpha 100))

;;; Font Setup

;; sample text:
;; | 中英文等宽的字体 |
;; | Mixed mono font  |
(progn
  (defun user/set-font (&rest args)
    (when (display-graphic-p)
      (set-frame-font "dejavu sans mono 10" t t)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "wenquanyi zen hei" :size 34)))))

  (user/set-font))

;;; Theme Setup
;; Face tweaks
(custom-set-faces
 '(yascroll:thumb-fringe ((t :background "#000000"
                             :foreground "#000000")))
 '(fringe ((t :background nil)))
 '(yas-field-highlight-face ((t :box "#777")))
 '(form-feed-line ((t :strike-through "#666"))))

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t))

;;; Mode Line Setup

(setq-default mode-line-format nil)

;;; Run setup for future frames.

(defun user/new-frame-setup (frame)
  (select-frame frame)
  (user/set-alpha)
  (user/set-font))

(add-hook 'after-make-frame-functions 'user/new-frame-setup)

(provide 'the-look-and-feel)
