;; Look & Feels

;; Font Setup

;; | 为了中英文等宽的字体     |
;; | For mixed monospace font |

(set-frame-font "inconsolata-11.5")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "wenquanyi micro hei" :size 34)))

;; Transparency Setup

(defun user/set-alpha (alpha)
  (set-frame-parameter (selected-frame) 'alpha (cons alpha alpha))
  (add-to-list 'default-frame-alist (cons 'alpha (cons alpha alpha))))

(user/set-alpha 100)

(setq x-underline-at-descent-line t)
(setq overline-margin 0)

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(custom-set-faces
 '(fringe ((t :background nil)))
 '(yas-field-highlight-face ((t :box "#777"))))

(provide 'the-look-and-feel)
