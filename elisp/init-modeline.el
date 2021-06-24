;; Mode Line  -*- lexical-binding: t; -*-

(setq-default header-line-format nil)

(+measure-time
 (window-width))

(defun +format-mode-line-simple ()
  (let* ((lhs '((:eval (meow-indicator))
                (:eval (rime-lighter))
                " Row %l Col %C"))
         (rhs '((:eval (+smart-file-name-cached))
                " "
                (:eval mode-name)))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(defun +format-mode-line-full ()
  (let* ((lhs '((:eval (meow-indicator))
                (:eval (rime-lighter))
                " Row %l Col %C  "
                (:eval (+smart-file-name-cached))))
         (rhs '((:eval mode-name)
                (vc-mode vc-mode)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))

(defun +format-mode-line ()
  (if (> (window-width) 80)
      (+format-mode-line-full)
    (+format-mode-line-simple)))

(setq-default mode-line-format '((:eval (+format-mode-line))))

(provide 'init-modeline)
