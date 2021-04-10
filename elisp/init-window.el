;;; -*- lexical-binding: t -*-

(defun +rotate-window ()
  (interactive)
  (let* ((wl (window-list nil nil (minibuffer-window)))
         (bl (reverse (mapcar (lambda (w) (window-buffer w)) wl)))
         (nbl (append (cdr bl) (list (car bl)))))
    (cl-loop for w in wl
          for b in (reverse nbl)
          do (set-window-buffer w b))
    (select-window (next-window))))

(provide 'init-window)
