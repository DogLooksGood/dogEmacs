(defvar +cursor-timer nil)

(defvar +cursor-color-index 0)
(setq +cursor-color-list
      '("#999999" "#AAAAAA" "#BBBBBB" "#CCCCCC" "#DDDDDD" "#EEEEEE" "#FFFFFF"
        "#FFFFFF" "#EEEEEE" "#DDDDDD" "#CCCCCC" "#BBBBBB" "#AAAAAA" "#999999"))

(defun +cursor-timer-function ()
  (if (>= (1+ +cursor-color-index) (length +cursor-color-list))
      (setq +cursor-color-index 0)
    (setq +cursor-color-index (1+ +cursor-color-index)))
  (set-cursor-color (nth +cursor-color-index +cursor-color-list)))

;; (setq +cursor-timer nil)
;; (cancel-timer +cursor-timer)

(unless +cursor-timer
  (setq +cursor-timer
        (run-with-timer 0 0.1 #'+cursor-timer-function)))

(blink-cursor-mode 1)

(provide 'init-cursor)
