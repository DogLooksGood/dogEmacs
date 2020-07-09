;; Common elisp packages

(use-package dash)
(use-package string-inflection)
(use-package seq)

(defmacro +measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(provide 'init-elisp)
