;; Mode Line  -*- lexical-binding: t; -*-

(setq-default header-line-format nil)

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                (:eval (rime-lighter))
                "Ln %l Col %C"))
         (rhs '((:eval (+smart-file-name-cached))
                " "
                mode-name
                (vc-mode vc-mode)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(setq-default mode-line-format
              (quote
               (:eval (+format-mode-line))))

(provide 'init-modeline)
