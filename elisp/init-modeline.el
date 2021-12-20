;; Mode Line  -*- lexical-binding: t; -*-

(defun +win-num ()
  (let ((n (window-numbering-get-number)))
    (alist-get
     n
     '((0 . "🄌")
       (1 . "❶")
       (2 . "❷")
       (3 . "❸")
       (4 . "❹")
       (5 . "❺")
       (6 . "❻")
       (7 . "❼")
       (8 . "❽")
       (9 . "❾")))))

(defun +format-mode-line ()
  (let* ((lhs '((:eval (when (bound-and-true-p window-numbering-mode) (concat " " (+win-num))))
                (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                (:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
                (:eval " L%l C%C")
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))
         (rhs '((:eval (+smart-file-name-cached))
                " "
                (:eval mode-name)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(setq-default mode-line-format '((:eval (+format-mode-line))))
(setq-default header-line-format nil)

(provide 'init-modeline)
