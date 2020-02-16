(defvar user/cursor-timer nil)

(defvar user/cursor-color-index 0)
;; (defvar user/cursor-color-list
;;   '("#000000" "#111111" "#222222" "#333333" "#444444" "#555555"
;;     "#666666" "#777777" "#888888" "#999999" "#AAAAAA" "#BBBBBB"
;;     "#CCCCCC" "#DDDDDD" "#EEEEEE" "#FFFFFF"
;;     "#FFFFFF" "#EEEEEE" "#DDDDDD" "#CCCCCC" "#BBBBBB" "#AAAAAA" "#999999" "#888888" "#777777" "#666666" "#555555" "#444444"
;;     "#333333" "#222222" "#111111" "#000000"))
(setq user/cursor-color-list
      '("#222222" "#444444" "#666666"
        "#888888" "#AAAAAA" "#CCCCCC"
        "#EEEEEE" "#FFFFFF" "#FFFFFF"
        "#DDDDDD" "#BBBBBB" "#999999"
        "#777777" "#555555" "#333333"))

(defun user/cursor-timer-function ()
  (if (>= (1+ user/cursor-color-index) (length user/cursor-color-list))
      (setq user/cursor-color-index 0)
    (setq user/cursor-color-index (1+ user/cursor-color-index)))
  (set-cursor-color (nth user/cursor-color-index user/cursor-color-list)))

;; (setq user/cursor-timer nil)
;; (cancel-timer user/cursor-timer)

(unless user/cursor-timer
  (setq user/cursor-timer
        (run-with-timer 0 0.05 #'user/cursor-timer-function)))

(blink-cursor-mode -1)

(provide 'the-cursor)
