(defvar user/cursor-timer nil)

(defvar user/cursor-color-index 0)
(setq user/cursor-color-list
      '("#999999" "#AAAAAA" "#BBBBBB" "#CCCCCC" "#DDDDDD" "#EEEEEE" "#FFFFFF"
        "#FFFFFF" "#EEEEEE" "#DDDDDD" "#CCCCCC" "#BBBBBB" "#AAAAAA" "#999999"))

(defun user/cursor-timer-function ()
  (if (>= (1+ user/cursor-color-index) (length user/cursor-color-list))
      (setq user/cursor-color-index 0)
    (setq user/cursor-color-index (1+ user/cursor-color-index)))
  (set-cursor-color (nth user/cursor-color-index user/cursor-color-list)))

;; (setq user/cursor-timer nil)
;; (cancel-timer user/cursor-timer)

(unless user/cursor-timer
  (setq user/cursor-timer
        (run-with-timer 0 0.1 #'user/cursor-timer-function)))

(blink-cursor-mode -1)

(provide 'the-cursor)
