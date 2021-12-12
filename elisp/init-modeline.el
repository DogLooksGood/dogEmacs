;; Mode Line  -*- lexical-binding: t; -*-

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                (:eval mode-line-position)
                (:eval (when (fboundp 'rime-lighter)
                         (rime-lighter)))
                (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))))
         (rhs '((:eval (+smart-file-name-cached))
                " "
                (:eval mode-name)
                (:eval (+vc-branch-name))))
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
