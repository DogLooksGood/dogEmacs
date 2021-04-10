;; Mode Line  -*- lexical-binding: t; -*-

;;; bench mark modeline.
;; (+measure-time (format-mode-line mode-line-format))

(defun +simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (string-width (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; (setq-default mode-line-format
;;               '((:eval
;;                  (+simple-mode-line-render
;;                   ;; left
;;                   '((:eval (meow-indicator))
;;                     " %l:%C "
;;                     (:propertize (-3 "%p") face +modeline-dim-face))
;;                   ;; right
;;                   '((:eval (rime-lighter))
;;                     " "
;;                     (:propertize mode-name face font-lock-keyword-face)
;;                     " "
;;                     (:eval (+smart-file-name-with-propertize))
;;                     " ")))))

(provide 'init-modeline)
